# Data organization and usage

This directory contains the data used in this project in two formats, Matlab 
(original format `.mat`) and a comma separated format (`.csv`) which is used by 
the functions in `src`.

----
Files in this directory include the experimental data in their original format
`.mat` and transformations to `.csv` used by the `.R` code in the `analysis` 
directory. Functions used for data reading and writing can be found in the 
`source` directory. 

- `matlab-files`: Original data files in `.mat` format.
  1. `LeeNavarro2002Data.mat`: Data from categorization experiment (all 4 
  conditions) reported in
  
  Lee, M. D., & Navarro, D. J. (2002). Extending the ALCOVE model of category 
  learning to featural stimulus domains. *Psychonomic Bulletin & Review*, 9(1), 
  43–58.
  
  2. `Lewandowsky2011Data.mat`: Data from categorization experiment (all 6
  conditions) reported in
  Lewandowsky S. (2011). Working memory capacity and categorization: individual 
  differences and modeling. *Journal of Experimental Psychology: Learning, 
  Memory and Cognition*. 37(3), 720-738. 
----

## Followig steps

- Missing files for all and type 6 from lewandowsky.

- Similarity metrics could also be stored as data but not sure they should be 
with the rest of the files. I could add a different directory and save into it
using another function.