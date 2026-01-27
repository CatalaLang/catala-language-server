import * as vscode from 'vscode';

class Logger {
  private static instance: Logger;
  private outputChannel: vscode.OutputChannel;

  private constructor() {
    this.outputChannel = vscode.window.createOutputChannel('Catala');
  }

  public static getInstance(): Logger {
    if (!Logger.instance) {
      Logger.instance = new Logger();
    }
    return Logger.instance;
  }

  public log(message: string): void {
    const timestamp = new Date().toISOString();
    this.outputChannel.appendLine(`[${timestamp}] ${message}`);
  }

  public show(): void {
    this.outputChannel.show();
  }

  public dispose(): void {
    this.outputChannel.dispose();
  }
}

export const logger = Logger.getInstance();
