import sys
import os
import glob


def extract_text_maptask(input_folder: str):
    """
    input_folder: the folder that contains all text transcripts files for MapTask corpus.
        e.g., data/text/Transcripts/
    Function:
        take each transcript file as input, go through each line, and extract the pure utterance text per line.
        I.e., removing the heading 'g' and 'f' that indicate the speaker roles.
    """
    file_names = glob.glob(os.path.join(input_folder, 'q*c*.txt'))
    for fn in file_names:
        fn_out = fn[:-4] + '_text.txt'
        with open(fn, 'r') as fr, open(fn_out, 'w') as fw:
            # skip the first 3 lines
            for _ in range(3):
                fr.readline()
            for line in fr:
                text = line[2:].strip()
                fw.write(text + '\n')


if __name__ == "__main__":
    extract_text_maptask(input_folder='data/text/HCRC Map Task Corpus/Transcripts')