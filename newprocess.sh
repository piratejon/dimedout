#!/bin/bash

src="+@ TITUS ANDRONICUS - 'DIMED OUT' ( Official Lyric Video )-0dX6LL7b8Y0.mkv"

[ -f "${src}" ] || youtube-dl 'https://www.youtube.com/watch?v=0dX6LL7b8Y0' || exit

mkdir -p thumbs

[ `ls thumbs | wc -l` -gt "0" ] || ffmpeg -i "${src}" -vf scale="192x108",mpdecimate,setpts=N/FRAME_RATE/TB thumbs/thumb-%04d.png

echo '[' > thumbs.json
ls thumbs | while read thumb ; do
  echo "\"../thumbs/${thumb}\","
done >> thumbs.json
truncate -s-2 thumbs.json
echo ']' >> thumbs.json
