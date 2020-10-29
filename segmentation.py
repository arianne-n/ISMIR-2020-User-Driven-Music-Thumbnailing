### Segmentation
### This file contains the code to obtain six fragments from songs in a corpus

from __future__ import print_function
# Music Structure Analysis Framework
import msaf
# Feature computation
import librosa
import seaborn as sns
# Goes through all files in a directory
import glob
# Creation of fragments
from pydub import AudioSegment
import random
import csv
# Regular expressions
import re

def structural_segmentation(audio_file):
	boundaries, labels = msaf.process(audio_file)
	return boundaries

def create_fragments(audio_file, boundaries):
	'''This function creates fragments for the given audio file directory 
	according to a list of onsets as starting points'''
	song = AudioSegment.from_wav(audio_file)
	preview_length = 9.95 * 1000
	last_possible = len(song) - preview_length

	# first remove irrelevant boundaries with fragments overlapping end of song
	boundaries = [int(boundary*1000) for boundary in boundaries if boundary*1000 < last_possible]

	song_ID = re.search('^/home/arianne/20190219/(.+?)\.wav', fname).group(1)
	print(song_ID)

	# create fragment starting at 1 minute in the song
	name = '/home/arianne/20190219/segments/selection/' + song_ID + "-" + "min.wav"
	create_wav(song, name, 60000, preview_length)

	# creat fragment starting at a random point in the song
	name = '/home/arianne/20190219/segments/selection/' + song_ID + "-" + "random.wav"
	base_case = random.sample(range(0, int(last_possible)), 1)[0]
	create_wav(song, name, base_case, preview_length)

	# select 4 structure boundaries, 1 minute in onset, and a random onset
	onsets = random.sample(boundaries, 4) + [60000] + [base_case]

	# create fragments starting at all detected structural boundaries
	for boundary in boundaries:
		print(boundary)
		if boundary in onsets:
			name = '/home/arianne/20190219/segments/selection/' + song_ID + "-" + str(boundary) + ".wav"
		else:
			name = '/home/arianne/20190219/segments/' + song_ID + "-" + str(boundary) + ".wav"
		create_wav(song, name, boundary, preview_length)
	
	return onsets

def create_wav(song, name, onset, preview_length):
	preview_end = onset + preview_length
	preview = song[onset:preview_end]
	preview.export(name, format="wav")

# Give path to directory and loop through files
path = "/home/arianne/20190219/*.wav"

with open('/home/arianne/20190219/chosen_onsets.csv', 'wb') as csvfile:
	spamwriter = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
	spamwriter.writerow(['ID', 'B1', 'B2', 'B3', 'B4', '1-MIN', 'RANDOM'])

	for fname in glob.glob(path):
		boundaries = structural_segmentation(fname)
		onsets = create_fragments(fname, boundaries)
		spamwriter.writerow([fname] + onsets)