# Source code

Files in this section include functions to generate read and write data files 
(`~/data-f`), calculate similarities (`~/similarity-f`) and sampling methods 
(`~/sampling-f`). Functions are used by files in the `~/analysis` directory.

----

### Data functions

The directory `~/data-f` contains functions that read and write data from the 
experiments in the project. Original data are stored in `.mat` format on 
`~/data/matlab-files`.

  - `lee-navarro-rw.R` this function reads data from Lee and Navarro's 2002 
  and saves the output in long format as a `.csv` file into the 
  `~data/csv-files` directory.
  
  - `lewandowsky-rw.R` this function reads data from Lewandowsky's 2011 
  and saves the output in long format as a `.csv` file into the 
  `~data/csv-files` directory.
  
----

### Similarity functions

The directory `~/similarity-f` contains functions that can write 
stimulus-features data files in `.csv` format and functions to calculate the 
similarity between two stimulus in an experiment. This functions are used by 
the sampling methods.

  - `lee-navarro-distinctive.R` function that takes a feature matrix and returns
  a multi-dimensional array containing 

----

TO DO

- Need a function that can save the stimulus features matrix from Lewandowsky's
experiment. Other functions should take a stimulus features and transform
them into a similarity matrix.

- Need a function that can take the stimulus index of an experiment and return 
a similarity matrix or a similarity in long format. First attempt could use 
lee and navarro's function.

- I need a function that can take the properties of the stimulus in each 
experiment and generate a similarity matrix. I think this will be
experiment dependent but it can be saved as a data file in a new directory.

