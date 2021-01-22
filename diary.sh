#!/bin/sh

DIARY_PATH=~/Documents/diary

currentDate=$(date +"%Y-%m-%d")
currentYear=$(date +"%Y")
fileName=${currentDate}.md
recordPath=${DIARY_PATH}/${currentYear}
recordFile=${recordPath}/${fileName}
if [ ! -d "$recordPath" ]
then
  mkdir "$recordPath"
fi
if [ ! -f "$recordFile" ]
then
  recordId=$(date +"%Y%m%d")
  echo "---\nid: $recordId\ndate: $currentDate\n---" > $recordFile
fi
time=$(date +"%T")
echo "\n### $time\n" >> $recordFile
vim "+norm G$" "+startinsert" $recordFile

