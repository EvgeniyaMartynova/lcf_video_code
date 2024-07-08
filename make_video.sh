#!/bin/bash

# Ensure the script stops on the first error encountered
set -e

FILE_NAME="video.mp4"

while [[ $# -gt 0 ]]; do
  key="$1"

  case $key in
    -fp|--frames_path)
      FRAMES_PATH="$2"
      shift # past argument
      shift # past value
      ;;
    -fn|--file_name)
      FILE_NAME="$2"
      shift # past argument
      shift # past value
      ;;
    *)    # unknown option
      shift # past argument
      ;;
  esac
done

# Define variables
frame_rate=20

# Get the dimensions of the first frame to adjust height if needed
first_frame="${FRAMES_PATH}/frame_001.png"
out_file_name="${FILE_NAME}.mp4"

dimensions=$(identify -format "%wx%h" "$first_frame")
width=$(echo $dimensions | cut -d'x' -f1)
height=$(echo $dimensions | cut -d'x' -f2)

# Adjust height to be divisible by 2
adjusted_height=$((height / 2 * 2))

# Run ffmpeg to create the video
ffmpeg -framerate $frame_rate -pattern_type glob -i "${FRAMES_PATH}/frame_*.png" -vf "scale=${width}:${adjusted_height}" -c:v libx264 -pix_fmt yuv420p -crf 18 -y $out_file_name

echo "Video created successfully: $out_file_name"
