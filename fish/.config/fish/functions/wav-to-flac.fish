function wav-to-flac
  for wav in *.{WAV,wav}
    set flac (string replace -ri 'wav$' 'flac' $wav)
    ffmpeg -i $wav $flac
  end
end
