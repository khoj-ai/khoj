// Audio Playback Processor for streaming audio playback with sample rate conversion
class AudioPlaybackProcessor extends AudioWorkletProcessor {
    constructor() {
        super();

        // Audio buffer to store incoming PCM data
        this.audioBuffer = null;
        this.bufferPosition = 0;

        // Sample rate conversion
        this.sourceSampleRate = 24000; // Input audio is 24kHz
        this.targetSampleRate = sampleRate; // AudioWorklet's native sample rate
        this.sampleRateRatio = this.sourceSampleRate / this.targetSampleRate;

        // Listen for audio data from main thread
        this.port.onmessage = (event) => {
            if (event.data.type === "audio-data") {
                this.appendAudioData(event.data.pcmData);
            } else if (event.data.type === "reset") {
                this.reset();
            }
        };
    }

    appendAudioData(newPcmData) {
        const newData = new Int16Array(newPcmData);

        if (!this.audioBuffer) {
            // Initialize with first chunk
            this.audioBuffer = new Int16Array(newData);
        } else {
            // Append new data to existing buffer
            const currentBuffer = this.audioBuffer;
            const combinedBuffer = new Int16Array(currentBuffer.length + newData.length);
            combinedBuffer.set(currentBuffer);
            combinedBuffer.set(newData, currentBuffer.length);
            this.audioBuffer = combinedBuffer;
        }
    }

    reset() {
        this.audioBuffer = null;
        this.bufferPosition = 0;
    }

    process(inputs, outputs) {
        const output = outputs[0];

        if (output.length > 0) {
            const outputChannel = output[0];

            if (this.audioBuffer && this.bufferPosition < this.audioBuffer.length) {
                // Fill output buffer with sample rate conversion
                const samplesNeeded = outputChannel.length;

                for (let i = 0; i < samplesNeeded; i++) {
                    // Calculate source position with sample rate conversion
                    const sourceIndex = Math.floor(this.bufferPosition + i * this.sampleRateRatio);

                    if (sourceIndex < this.audioBuffer.length) {
                        // Convert Int16 to Float32 and normalize
                        outputChannel[i] = this.audioBuffer[sourceIndex] / 32768.0;
                    } else {
                        outputChannel[i] = 0; // Fill with silence when no data
                    }
                }

                // Update position based on how much source data we consumed
                this.bufferPosition += Math.floor(samplesNeeded * this.sampleRateRatio);

                // Clean up consumed audio data to prevent memory growth
                if (this.bufferPosition > this.audioBuffer.length * 0.5) {
                    const remainingData = this.audioBuffer.slice(this.bufferPosition);
                    this.audioBuffer = remainingData;
                    this.bufferPosition = 0;
                }
            } else {
                // No audio data available, output silence
                outputChannel.fill(0);
            }
        }

        return true; // Keep the processor alive
    }
}

registerProcessor("audio-playback-processor", AudioPlaybackProcessor);
