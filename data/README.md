# Data organization and usage

Files in this directory include experimental data in their original format
(`.mat`) and in `.csv`, alongside experimental-stimulus features
used by functions in the `~/analysis` directory . Functions used for data 
reading and writing can be found in the `~/src` directory. 

----

`~/matlab-files`: directory contains original data files in `.mat` format. These
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
  
----

`~/csv-files`: directory contains data files on `.csv` format, these files are 
used by functions in the `~/analysis` directory.

  1. `lee-navarro-2002-all.csv` data from all conditions and participants in Lee 
  and Navarro (2002) stored in long format.
  
  2. `lee-navarro-2002-type4.csv` data from condition (type) 4 from Lee and 
  Navarro (2002) stored in long format.
  
  3. `lewandowsky-2011-all.csv` data from all participants and conditions 
  (types) in Lewandowsky (2011) stored in long format. 
  
  4. `lewandowsky-2011-type6.csv` data from all participants for condition 
  (type) 6 category structures in Lewandowsky (2011) stored in long 
  format.
  
----

`~/stimulus-features`: this directory contains stimulus feature matrices from 
the experiments analyzed in the project in csv format.

  1. `lee-navarro-features.csv` features (presence/absence) of stimulus used in 
  Lee and Navarro's 2002 experiment.
  
  2. `lewandowsky-features.csv` features (presence/absence) of stimulus used in 
  Lewandowsky's 2011 experiment.
