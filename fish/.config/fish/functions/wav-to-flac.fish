function wav-to-flac
  # use fd for parallelization
  echo "Converting" (count **/*.{wav,WAV}) "files..."
  fd --type f --extension wav --exec \
      ffmpeg -i "{}" \
      # run quietly, but show conversion progress
      -loglevel warning \
      -stats \
      "{.}.flac"
end
