function wav-to-flac
  for wav in *.{WAV,wav}
    set flac (echo $wav | sed 's/\.wav$/.flac/I')
    ffmpeg -i $wav $flac
  end
end
