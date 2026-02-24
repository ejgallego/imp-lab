import * as vscode from 'vscode'
import * as fs from 'node:fs'
import * as path from 'node:path'

class LeanToyDebugAdapterFactory implements vscode.DebugAdapterDescriptorFactory {
    constructor(private readonly output: vscode.OutputChannel) {}

    createDebugAdapterDescriptor(session: vscode.DebugSession): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
        const command = this.resolveToyDapCommand(session)
        const args = this.resolveToyDapArgs(session)
        this.output.appendLine(`[lean-toy-dap] launching ${command}${args.length > 0 ? ` ${args.join(' ')}` : ''}`)
        return new vscode.DebugAdapterExecutable(command, args)
    }

    private resolveToyDapCommand(session: vscode.DebugSession): string {
        const configured = session.configuration?.toydapPath
        if (typeof configured === 'string' && configured.trim().length > 0) {
            return configured.trim()
        }

        const workspaceFolders = vscode.workspace.workspaceFolders ?? []
        for (const folder of workspaceFolders) {
            const candidate = path.join(folder.uri.fsPath, '.lake', 'build', 'bin', 'toydap')
            if (fs.existsSync(candidate)) {
                return candidate
            }
        }

        return 'toydap'
    }

    private resolveToyDapArgs(session: vscode.DebugSession): string[] {
        const raw = session.configuration?.toydapArgs
        if (!Array.isArray(raw)) {
            return []
        }
        return raw.map(value => String(value))
    }
}

class LeanToyDebugConfigurationProvider implements vscode.DebugConfigurationProvider {
    constructor(private readonly output: vscode.OutputChannel) {}

    private tryLoadJson(filePath: string): unknown | undefined {
        if (!fs.existsSync(filePath)) {
            return undefined
        }
        const raw = fs.readFileSync(filePath, 'utf8')
        return JSON.parse(raw)
    }

    resolveDebugConfiguration(
        folder: vscode.WorkspaceFolder | undefined,
        config: vscode.DebugConfiguration,
    ): vscode.ProviderResult<vscode.DebugConfiguration> {
        if (!config.type) {
            config.type = 'lean-toy-dap'
        }
        if (!config.request) {
            config.request = 'launch'
        }
        if (!config.name) {
            config.name = 'ImpLab Toy DAP'
        }
        if (!config.source) {
            const active = vscode.window.activeTextEditor?.document.uri
            if (active?.scheme === 'file') {
                config.source = active.fsPath
            }
        }
        if (!config.programInfo) {
            const workspaceRoot = folder?.uri.fsPath ?? vscode.workspace.workspaceFolders?.[0]?.uri.fsPath
            const generatedPath = workspaceRoot
                ? path.join(workspaceRoot, '.dap', 'programInfo.generated.json')
                : undefined
            if (generatedPath) {
                try {
                    const loaded = this.tryLoadJson(generatedPath)
                    if (loaded !== undefined) {
                        config.programInfo = loaded
                        this.output.appendLine(`[lean-toy-dap] launch.programInfo loaded from ${generatedPath}`)
                    }
                } catch (err) {
                    vscode.window.showErrorMessage(`lean-toy-dap: invalid JSON in ${generatedPath}`)
                    return null
                }
            }
        }
        if (!config.programInfo) {
            vscode.window.showErrorMessage(
                "lean-toy-dap launch requires 'programInfo'. Run `lake exe dap-export --decl ImpLab.Lang.Examples.mainProgram --out .dap/programInfo.generated.json` or set launch.programInfo.",
            )
            return null
        }
        return config
    }
}

export function activate(context: vscode.ExtensionContext): void {
    const output = vscode.window.createOutputChannel('ImpLab Toy DAP')

    const configProvider = new LeanToyDebugConfigurationProvider(output)
    const adapterFactory = new LeanToyDebugAdapterFactory(output)

    context.subscriptions.push(
        output,
        vscode.debug.registerDebugConfigurationProvider('lean-toy-dap', configProvider),
        vscode.debug.registerDebugAdapterDescriptorFactory('lean-toy-dap', adapterFactory),
        vscode.commands.registerCommand('impLab.startDebugging', async () => {
            const active = vscode.window.activeTextEditor?.document.uri
            const source = active?.scheme === 'file' ? active.fsPath : undefined
            await vscode.debug.startDebugging(undefined, {
                name: 'ImpLab Toy DAP',
                type: 'lean-toy-dap',
                request: 'launch',
                source,
                stopOnEntry: true,
            })
        }),
    )
}

export function deactivate(): void {}
