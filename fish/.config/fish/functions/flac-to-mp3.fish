function flac-to-mp3
  for i in **/*.{flac,FLAC}
    set o (string replace -ri 'flac$' 'mp3' $i)
    ffmpeg -i $i -codec:a libmp3lame -q:a 2 $o
  end
end
