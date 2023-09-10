function flac-to-mp3
  # use fd for parallelization
  echo "Converting" (count **/*.{flac,FLAC}) "files..."
  fd --type f --extension flac --exec \
      ffmpeg -i "{}" -codec:a libmp3lame -qscale:a 2 \
      # run quietly, but show conversion progress
      -loglevel warning -stats \
      "{.}.mp3"
end
