// PCM Audio Processor to convert float32 audio to int16 PCM format
class PCMProcessor extends AudioWorkletProcessor {
    constructor() {
        super();

        // Buffer for accumulating audio data
        this.bufferSize = 4096; // Buffer size for processing chunks
        this.buffer = new Float32Array(this.bufferSize);
        this.bufferIndex = 0;

        // Store the actual sample rate for MIME type
        this.actualSampleRate = sampleRate; // AudioWorklet's native sample rate
    }

    process(inputs, outputs) {
        const input = inputs[0];

        if (input.length > 0) {
            const inputChannel = input[0]; // Get the first (mono) channel

            // Accumulate samples in buffer
            for (let i = 0; i < inputChannel.length; i++) {
                this.buffer[this.bufferIndex] = inputChannel[i];
                this.bufferIndex++;

                // When buffer is full, convert to PCM and send to main thread
                if (this.bufferIndex >= this.bufferSize) {
                    this.sendPCMData();
                    this.bufferIndex = 0;
                }
            }
        }

        return true; // Keep the processor alive
    }

    sendPCMData() {
        // Convert float32 (-1.0 to 1.0) to int16 PCM (-32768 to 32767)
        const pcmData = new Int16Array(this.bufferSize);

        for (let i = 0; i < this.bufferSize; i++) {
            // Clamp the sample to [-1, 1] range
            const sample = Math.max(-1, Math.min(1, this.buffer[i]));

            // Convert to 16-bit PCM
            pcmData[i] = sample < 0 ? sample * 0x8000 : sample * 0x7fff;
        }

        // Send the PCM data and sample rate to the main thread
        this.port.postMessage({
            type: "pcm-data",
            data: pcmData.buffer,
            sampleRate: this.actualSampleRate,
        });
    }
}

registerProcessor("pcm-processor", PCMProcessor);
