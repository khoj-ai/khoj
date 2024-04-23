module.exports = {
    apps: [
      {
        name: 'open_interpreter',
        script: 'poetry',
        args: 'run 01 --local --stt-service local-whisper --tts-service piper',
        interpreter: 'bash',
      },

      {
        name: 'SoLoAI',
        script: 'khoj',
        args: '--anonymous-mode',
        interpreter: 'bash',
      },
    ],
  };