const { spawn } = require('child_process');

class ChessEngine {
    constructor(enginePath) {
        this.engine = spawn(enginePath);

        this.engine.stdout.setEncoding('utf8');
        this.engine.stdin.setDefaultEncoding('utf8');

        this.stdoutBuffer = '';
        this.engine.stdout.on('data', (data) => {
            this.stdoutBuffer += data;
            // Optionally, emit events or process lines here
        });

        this.engine.stderr.on('data', (data) => {
            console.error('Engine stderr:', data.toString());
        });

        this.engine.on('close', (code) => {
            console.log(`Engine exited with code ${code}`);
        });
    }

    sendCommand(command) {
        this.engine.stdin.write(command + '\n');
    }

    async getResponse(timeout = 1000) {
        // Wait for output (simple version: wait for a short time)
        await new Promise(resolve => setTimeout(resolve, timeout));
        const output = this.stdoutBuffer;
        this.stdoutBuffer = '';
        return output;
    }

    async sendAndGet(command, timeout = 1000) {
        this.sendCommand(command);
        return await this.getResponse(timeout);
    }

    quit() {
        this.sendCommand('quit');
        this.engine.stdin.end();
    }
}

// Example usage:
(async () => {
    const engine = new ChessEngine('./mahoraga.exe'); // or 'mahoraga.exe' on Windows

    // UCI initialization
    console.log(await engine.sendAndGet('isready', 5000));

    // Set position and search
    await engine.sendAndGet('ucinewgame');
    await engine.sendAndGet('position startpos');
    const output = await engine.sendAndGet('go depth 5', 5000); // Wait longer for search
    console.log(output);

    engine.quit();
})();
