import * as fs from 'node:fs/promises'
import * as path from 'node:path'
import * as vscode from 'vscode'
import { LeanRpcSession } from './leanRpc'

type RequestMessage = {
    seq: number
    command: string
    arguments?: any
}

type ControlResponse = {
    line: number
    stopReason: string
    terminated: boolean
}

type LaunchResponse = {
    sessionId: number
    threadId: number
    line: number
    stopReason: string
    terminated: boolean
}

type BreakpointView = {
    line: number
    verified: boolean
    message?: string
}

type SetBreakpointsResponse = {
    breakpoints: BreakpointView[]
}

type ThreadsResponse = {
    threads: { id: number; name: string }[]
}

type StackTraceResponse = {
    totalFrames: number
    stackFrames: { id: number; name: string; line: number; column: number }[]
}

type ScopesResponse = {
    scopes: { name: string; variablesReference: number; expensive: boolean }[]
}

type VariablesResponse = {
    variables: { name: string; value: string; variablesReference: number }[]
}

function normalizeStoppedReason(reason: string): 'entry' | 'step' | 'breakpoint' | 'pause' {
    switch (reason) {
        case 'entry':
        case 'step':
        case 'breakpoint':
        case 'pause':
            return reason
        default:
            return 'pause'
    }
}

function escapeRegExp(text: string): string {
    return text.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')
}

export class LeanToyDebugAdapter implements vscode.DebugAdapter {
    private readonly emitter = new vscode.EventEmitter<any>()
    readonly onDidSendMessage = this.emitter.event

    private nextSeq = 1
    private rpcSession: LeanRpcSession | undefined
    private dapSessionId: number | undefined
    private sourcePath: string | undefined
    private readonly threadId = 1
    private pendingBreakpoints: number[] = []

    constructor(private readonly output: vscode.OutputChannel) {}

    dispose(): void {
        this.rpcSession?.dispose()
        this.rpcSession = undefined
    }

    handleMessage(message: any): void {
        if (message?.type !== 'request') {
            return
        }
        void this.handleRequest(message as RequestMessage)
    }

    private send(message: any): void {
        this.emitter.fire({ ...message, seq: this.nextSeq++ })
    }

    private sendResponse(request: RequestMessage, body: any = {}): void {
        this.send({
            type: 'response',
            request_seq: request.seq,
            success: true,
            command: request.command,
            body,
        })
    }

    private sendErrorResponse(request: RequestMessage, error: unknown): void {
        const message = error instanceof Error ? error.message : String(error)
        this.send({
            type: 'response',
            request_seq: request.seq,
            success: false,
            command: request.command,
            message,
            body: {
                error: {
                    id: 1,
                    format: message,
                },
            },
        })
    }

    private sendEvent(event: string, body: any = {}): void {
        this.send({
            type: 'event',
            event,
            body,
        })
    }

    private ensureSourcePath(args: any): string {
        if (typeof args?.source === 'string' && args.source.length > 0) {
            return args.source
        }
        const active = vscode.window.activeTextEditor?.document.uri
        if (active?.scheme === 'file') {
            return active.fsPath
        }
        throw new Error("Missing launch field 'source' and no active file editor available")
    }

    private async readProgram(args: any): Promise<unknown[] | undefined> {
        if (Array.isArray(args?.program)) {
            return args.program
        }
        if (typeof args?.programFile === 'string' && args.programFile.length > 0) {
            const file = args.programFile
            const raw = await fs.readFile(file, 'utf8')
            const parsed = JSON.parse(raw)
            if (!Array.isArray(parsed)) {
                throw new Error(`Program file '${file}' must contain a JSON array`)
            }
            return parsed
        }
        return undefined
    }

    private async resolveLaunchPosition(
        sourcePath: string,
        entryPoint: string,
    ): Promise<{ line: number; character: number }> {
        const sourceUri = vscode.Uri.file(sourcePath)
        const active = vscode.window.activeTextEditor
        const doc =
            active && active.document.uri.toString() === sourceUri.toString()
                ? active.document
                : await vscode.workspace.openTextDocument(sourceUri)

        const entrySimple = entryPoint.split('.').filter(part => part.length > 0).at(-1) ?? entryPoint
        const declPattern = new RegExp(`^\\s*(def|abbrev)\\s+${escapeRegExp(entrySimple)}\\b`)
        for (let line = 0; line < doc.lineCount; line++) {
            const text = doc.lineAt(line).text
            if (declPattern.test(text)) {
                const character = Math.max(text.indexOf(entrySimple), 0)
                return { line, character }
            }
        }

        if (active && active.document.uri.toString() === sourceUri.toString()) {
            const pos = active.selection.active
            return { line: pos.line, character: pos.character }
        }

        const line = Math.max(doc.lineCount - 1, 0)
        const character = doc.lineAt(line).text.length
        return { line, character }
    }

    private async callLean<T>(method: string, params: unknown): Promise<T> {
        if (!this.rpcSession) {
            throw new Error('Lean RPC session is not connected')
        }
        return this.rpcSession.call<T>(method, params)
    }

    private async callSessionMethod<T>(method: string, extraParams: Record<string, unknown> = {}): Promise<T> {
        if (this.dapSessionId === undefined) {
            throw new Error('No active DAP session. Launch first.')
        }
        return this.callLean<T>(method, { sessionId: this.dapSessionId, ...extraParams })
    }

    private emitStopOrTerminate(control: ControlResponse): void {
        if (control.terminated || control.stopReason === 'terminated') {
            this.sendEvent('terminated', {})
            return
        }
        this.sendEvent('stopped', {
            reason: normalizeStoppedReason(control.stopReason),
            threadId: this.threadId,
            allThreadsStopped: true,
        })
    }

    private async handleInitialize(request: RequestMessage): Promise<void> {
        this.sendResponse(request, {
            supportsConfigurationDoneRequest: true,
            supportsStepBack: true,
            supportsRestartRequest: false,
        })
        this.sendEvent('initialized', {})
    }

    private async handleLaunch(request: RequestMessage): Promise<void> {
        const args = request.arguments ?? {}
        const sourcePath = this.ensureSourcePath(args)
        const program = await this.readProgram(args)
        const stopOnEntry = args?.stopOnEntry !== false
        const entryPoint =
            typeof args?.entryPoint === 'string' && args.entryPoint.trim().length > 0
                ? args.entryPoint.trim()
                : 'mainProgram'
        const position = await this.resolveLaunchPosition(sourcePath, entryPoint)

        this.sourcePath = sourcePath
        this.rpcSession?.dispose()
        this.rpcSession = await LeanRpcSession.connect(vscode.Uri.file(sourcePath))
        this.output.appendLine(
            `[lean-toy-dap] launch source=${sourcePath} entry=${entryPoint} line=${position.line + 1}`,
        )
        const launch =
            program !== undefined
                ? await this.callLean<LaunchResponse>('Dap.Server.dapLaunch', {
                      program,
                      stopOnEntry,
                      breakpoints: this.pendingBreakpoints,
                  })
                : await this.callLean<LaunchResponse>('Dap.Server.dapLaunchMain', {
                      entryPoint,
                      line: position.line,
                      character: position.character,
                      stopOnEntry,
                      breakpoints: this.pendingBreakpoints,
                  })

        this.dapSessionId = launch.sessionId
        this.sendResponse(request)

        if (launch.terminated || launch.stopReason === 'terminated') {
            this.sendEvent('terminated', {})
        } else {
            this.sendEvent('stopped', {
                reason: normalizeStoppedReason(launch.stopReason),
                threadId: this.threadId,
                allThreadsStopped: true,
            })
        }
    }

    private async handleSetBreakpoints(request: RequestMessage): Promise<void> {
        const args = request.arguments ?? {}
        const lines: number[] = (args?.breakpoints ?? [])
            .map((bp: any) => Number(bp?.line))
            .filter((line: number) => Number.isFinite(line) && line > 0)
        this.pendingBreakpoints = lines

        if (this.dapSessionId === undefined) {
            const fallback = lines.map(line => ({ verified: true, line }))
            this.sendResponse(request, { breakpoints: fallback })
            return
        }

        const result = await this.callSessionMethod<SetBreakpointsResponse>('Dap.Server.dapSetBreakpoints', {
            breakpoints: lines,
        })
        const breakpoints = result.breakpoints.map(bp => ({
            verified: bp.verified,
            line: bp.line,
            message: bp.message,
        }))
        this.sendResponse(request, { breakpoints })
    }

    private async handleThreads(request: RequestMessage): Promise<void> {
        if (this.dapSessionId === undefined) {
            this.sendResponse(request, { threads: [{ id: this.threadId, name: 'main' }] })
            return
        }
        const result = await this.callSessionMethod<ThreadsResponse>('Dap.Server.dapThreads')
        this.sendResponse(request, { threads: result.threads })
    }

    private async handleStackTrace(request: RequestMessage): Promise<void> {
        const args = request.arguments ?? {}
        const result = await this.callSessionMethod<StackTraceResponse>('Dap.Server.dapStackTrace', {
            startFrame: Number(args?.startFrame ?? 0),
            levels: Number(args?.levels ?? 20),
        })
        const sourcePath = this.sourcePath
        const source = sourcePath
            ? { name: path.basename(sourcePath), path: sourcePath }
            : undefined
        const stackFrames = result.stackFrames.map(frame => ({
            id: frame.id,
            name: frame.name,
            line: frame.line,
            column: frame.column,
            source,
        }))
        this.sendResponse(request, {
            stackFrames,
            totalFrames: result.totalFrames,
        })
    }

    private async handleScopes(request: RequestMessage): Promise<void> {
        const args = request.arguments ?? {}
        const frameId = Number(args?.frameId ?? 0)
        const result = await this.callSessionMethod<ScopesResponse>('Dap.Server.dapScopes', { frameId })
        this.sendResponse(request, { scopes: result.scopes })
    }

    private async handleVariables(request: RequestMessage): Promise<void> {
        const args = request.arguments ?? {}
        const variablesReference = Number(args?.variablesReference ?? 0)
        const result = await this.callSessionMethod<VariablesResponse>('Dap.Server.dapVariables', {
            variablesReference,
        })
        this.sendResponse(request, { variables: result.variables })
    }

    private async handleNext(request: RequestMessage): Promise<void> {
        const result = await this.callSessionMethod<ControlResponse>('Dap.Server.dapNext')
        this.sendResponse(request)
        this.emitStopOrTerminate(result)
    }

    private async handleStepBack(request: RequestMessage): Promise<void> {
        const result = await this.callSessionMethod<ControlResponse>('Dap.Server.dapStepBack')
        this.sendResponse(request)
        this.emitStopOrTerminate(result)
    }

    private async handleContinue(request: RequestMessage): Promise<void> {
        this.sendEvent('continued', { threadId: this.threadId, allThreadsContinued: true })
        const result = await this.callSessionMethod<ControlResponse>('Dap.Server.dapContinue')
        this.sendResponse(request, { allThreadsContinued: true })
        this.emitStopOrTerminate(result)
    }

    private async handlePause(request: RequestMessage): Promise<void> {
        const result = await this.callSessionMethod<ControlResponse>('Dap.Server.dapPause')
        this.sendResponse(request)
        this.emitStopOrTerminate(result)
    }

    private async handleDisconnect(request: RequestMessage): Promise<void> {
        if (this.dapSessionId !== undefined) {
            try {
                await this.callSessionMethod('Dap.Server.dapDisconnect')
            } catch {
                // Disconnect should be best-effort.
            }
        }
        this.rpcSession?.dispose()
        this.rpcSession = undefined
        this.dapSessionId = undefined
        this.sendResponse(request)
    }

    private async handleRequest(request: RequestMessage): Promise<void> {
        try {
            switch (request.command) {
                case 'initialize':
                    await this.handleInitialize(request)
                    break
                case 'launch':
                    await this.handleLaunch(request)
                    break
                case 'setBreakpoints':
                    await this.handleSetBreakpoints(request)
                    break
                case 'configurationDone':
                    this.sendResponse(request)
                    break
                case 'threads':
                    await this.handleThreads(request)
                    break
                case 'stackTrace':
                    await this.handleStackTrace(request)
                    break
                case 'scopes':
                    await this.handleScopes(request)
                    break
                case 'variables':
                    await this.handleVariables(request)
                    break
                case 'next':
                    await this.handleNext(request)
                    break
                case 'stepBack':
                    await this.handleStepBack(request)
                    break
                case 'continue':
                    await this.handleContinue(request)
                    break
                case 'pause':
                    await this.handlePause(request)
                    break
                case 'disconnect':
                case 'terminate':
                    await this.handleDisconnect(request)
                    break
                default:
                    this.sendErrorResponse(request, `Unsupported request: ${request.command}`)
                    break
            }
        } catch (err) {
            this.output.appendLine(`[lean-toy-dap] ${request.command} failed: ${String(err)}`)
            this.sendErrorResponse(request, err)
        }
    }
}
