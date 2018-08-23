#!/bin/bash

src="+@ TITUS ANDRONICUS - 'DIMED OUT' ( Official Lyric Video )-0dX6LL7b8Y0.mkv"

deciframes=deciframes
undeciframes=undeciframes
unuseddeci="unused${deciframes}"
unusedundeci="unused${undeciframes}"
thumbs=thumbs

youtube-dl 'https://www.youtube.com/watch?v=0dX6LL7b8Y0'
mkdir -p "${deciframes}" "${undeciframes}" "${unuseddeci}" "${unusedundeci}" "${thumbs}"
rm -rf "${deciframes}"/* "${undeciframes}"/* "${unuseddeci}"/* "${unusedundeci}"/* "${thumbs}"/*
ffmpeg -i "${src}" -vf mpdecimate,setpts=N/FRAME_RATE/TB "${deciframes}/%04d.jpg"
ffmpeg -i "${src}" "${undeciframes}/%04d.jpg"

ls "${deciframes}" | while read file ; do grep -q `basename "${file}" .jpg` decimatedindex.txt || mv "${deciframes}/${file}" "${unuseddeci}" ; done

md5sum "${deciframes}"/* | tee "${deciframes}.md5.txt" | sort > "${deciframes}.md5.sorted.txt"
md5sum "${undeciframes}"/* | tee "${undeciframes}.md5.txt" | sort > "${undeciframes}.md5.sorted.txt"

join -a1 "${deciframes}.md5.sorted.txt" "${undeciframes}.md5.sorted.txt" | cut -d' ' -f3 | sort | sed 's/.*[^0-9]\([0-9]\+\).*/\1/g' > globalindex.txt

ls "${undeciframes}" | while read file ; do grep -q `basename "${file}" .jpg` globalindex.txt || mv "${undeciframes}/${file}" "${unusedundeci}" ; done

# <https://askubuntu.com/a/468003/859470> 2018-08-22
fps=`cat <(echo scale=2) <(ffprobe -v 0 -of csv=p=0 -select_streams 0 -show_entries stream=r_frame_rate "${src}") | bc`

t0=0
echo -n convert ""> cvt_command.sh
while read frame ; do
  t1=$frame
  dur=`echo "(100*($t1 - $t0))/$fps" | bc`
  echo -delay $dur -resize 480x270 \"${undeciframes}/${frame}.jpg\" "\\"
  t0=$t1
done < globalindex.txt >> cvt_command.sh
echo timedout.gif >> cvt_command.sh
. cvt_command.sh

echo '[' > thumbs.json
ls "${undeciframes}" | while read img ; do
  convert "${undeciframes}/${img}" -resize 15% "${thumbs}/${img}"
  echo "\"${thumbs}/${img}\","
done >> thumbs.json
truncate -s-2 thumbs.json
echo ']' >> thumbs.json

