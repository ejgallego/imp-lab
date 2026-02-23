import * as vscode from 'vscode'

const LEAN_EXTENSION_ID = 'leanprover.lean4'
const KEEP_ALIVE_PERIOD_MS = 10_000

type LeanClientLike = {
    sendRequest(method: string, params: unknown): Promise<any>
    sendNotification?(method: string, params: unknown): Promise<void>
}

type LeanClientProviderLike = {
    getActiveClient?: () => LeanClientLike | undefined
}

type Lean4ExportsLike = {
    lean4EnabledFeatures: Promise<{ clientProvider: LeanClientProviderLike }>
}

function delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms))
}

async function getLeanClient(sourceUri: vscode.Uri): Promise<LeanClientLike> {
    const leanExt = vscode.extensions.getExtension<Lean4ExportsLike>(LEAN_EXTENSION_ID)
    if (!leanExt) {
        throw new Error(`Extension '${LEAN_EXTENSION_ID}' is not installed`)
    }

    const api = await leanExt.activate()
    const enabled = await api.lean4EnabledFeatures
    const provider = enabled.clientProvider

    let client = provider.getActiveClient?.()
    if (client) {
        return client
    }

    await vscode.window.showTextDocument(sourceUri, { preserveFocus: true, preview: false })
    for (let i = 0; i < 20; i++) {
        await delay(100)
        client = provider.getActiveClient?.()
        if (client) {
            return client
        }
    }

    throw new Error('Unable to obtain an active Lean language client. Open and focus a Lean file in this project.')
}

export class LeanRpcSession implements vscode.Disposable {
    private readonly client: LeanClientLike
    private readonly sourceUri: vscode.Uri
    private readonly sessionId: string
    private readonly keepAliveHandle: NodeJS.Timeout
    private disposed = false

    private constructor(client: LeanClientLike, sourceUri: vscode.Uri, sessionId: string) {
        this.client = client
        this.sourceUri = sourceUri
        this.sessionId = sessionId
        this.keepAliveHandle = setInterval(() => {
            void this.keepAlive()
        }, KEEP_ALIVE_PERIOD_MS)
    }

    static async connect(sourceUri: vscode.Uri): Promise<LeanRpcSession> {
        const client = await getLeanClient(sourceUri)
        const connected = await client.sendRequest('$/lean/rpc/connect', { uri: sourceUri.toString() })
        const rawSessionId = connected?.sessionId
        if (!(typeof rawSessionId === 'string' || typeof rawSessionId === 'number')) {
            throw new Error(`Invalid RPC connect response: ${JSON.stringify(connected)}`)
        }
        const sessionId = String(rawSessionId)
        return new LeanRpcSession(client, sourceUri, sessionId)
    }

    async call<T>(method: string, params: unknown, position?: vscode.Position): Promise<T> {
        const pos = position ?? new vscode.Position(0, 0)
        const rpcCallParams = {
            textDocument: { uri: this.sourceUri.toString() },
            position: { line: pos.line, character: pos.character },
            sessionId: this.sessionId,
            method,
            params,
        }
        return this.client.sendRequest('$/lean/rpc/call', rpcCallParams) as Promise<T>
    }

    get uri(): vscode.Uri {
        return this.sourceUri
    }

    get id(): number {
        return Number(this.sessionId)
    }

    private async keepAlive(): Promise<void> {
        if (this.disposed || !this.client.sendNotification) {
            return
        }
        try {
            await this.client.sendNotification('$/lean/rpc/keepAlive', {
                uri: this.sourceUri.toString(),
                sessionId: this.sessionId,
            })
        } catch {
            // The adapter will discover dead sessions on the next call.
        }
    }

    dispose(): void {
        if (this.disposed) {
            return
        }
        this.disposed = true
        clearInterval(this.keepAliveHandle)
    }
}
