# Data organization and usage

This directory contains the data used in this project in two formats, Matlab 
(original format `.mat`) and a comma separated format (`.csv`) which is used by 
the functions in `~/src`.

----
Files in this directory include the experimental data in their original format
`.mat` and transformations to `.csv` used by the `.R` code in `~/analysis` 
directory. Functions used for data reading and writing can be found in the 
`~/source` directory. 

- `matlab-files`: directory contains original data files in `.mat` format. These
files are **only** used by functions that read and write data into other 
formats.

  1. `LeeNavarro2002Data.mat` Data from categorization experiment (all 4 
  conditions) reported in:
  
  Lee, M. D., & Navarro, D. J. (2002). Extending the ALCOVE model of category 
  learning to featural stimulus domains. *Psychonomic Bulletin & Review*, 9(1), 
  43–58.
  
  2. `Lewandowsky2011Data.mat` Data from categorization experiment (all 6
  conditions) reported in:
  
  Lewandowsky S. (2011). Working memory capacity and categorization: individual 
  differences and modeling. *Journal of Experimental Psychology: Learning, 
  Memory and Cognition*. 37(3), 720-738.
  
- `csv-files`: directory contains data files on `.csv` format, these files are 
used by files in `~/analysis`.

  1. `lee-navarro-2002-all.csv` data from all conditions and participants in Lee 
  and Navarro (2002) stored in long format.
  
  2. `lee-navarro-2002-type4.csv` data from condition (type) 4 from Lee and Navarro
  (2002) stored in long format.
  
----

## Followig steps

- Missing files for all and type 6 from lewandowsky.

- Need to write a function that creates a data file in long format that contains
the condition x stimulus x category lebel from the lewandowsky 2011 experiment 
as some of the information is missing from the original file.

- Similarity metrics could also be stored as data but not sure they should be 
with the rest of the files. I could add a different directory and save into it
using another function.