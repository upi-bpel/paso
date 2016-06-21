# PASO
This repository contains the source code of an F\# implementation of the PASO probabilistic approach for predicting QoS of BPEL service orchestrations. Such approach has been presented in
> _L. Bartoloni, A. Brogi, A. Ibrahim <br>
> **Probabilistic Prediction of the QoS of Service Orchestrations: A Truly Compositional Approach.** <br>
> Published in ICSOC 2014, Service-Oriented Computing, pp. 378-385. Springer Berlin Heidelberg, 2014.<br>_ 

If you wish to reuse the sources in this repository, please properly cite the above mentioned paper. Below you can find the BibTex reference:
```
@inproceedings{PASO,
  author    = {Leonardo Bartoloni and Antonio Brogi and Ahmad Ibrahim},
  title     = {Probabilistic Prediction of the QoS of Service Orchestrations: {A} Truly Compositional Approach},
  booktitle = {Service-Oriented Computing - 12th International Conference, {ICSOC} 2014, Paris, France, November 3-6, 2014. Proceedings},
  pages     = {378--385},
  year      = {2014},
  editor    = {Xavier Franch and Aditya K. Ghose and Grace A. Lewis and Sami Bhiri},
  series    = {Lecture Notes in Computer Science},
  volume    = {8831},
  publisher = {Springer},
  url       = {http://dx.doi.org/10.1007/978-3-662-45391-9},
  doi       = {10.1007/978-3-662-45391-9}
}
```
## How to use PASO
In order to use this program, you need to do following steps:

* Install **Microsoft Visual Studio 2013**. 
* Download and extract the release source code **"ieee-full.zip"**.
* Browse to **Code** -> **lx-bpel** and open **lx-bpel.sln** with Visual studio.
* Open the code **Program.fs** and set variable **data_path** to the directory containg WS-BPEL example and Annotation files. 
  * Set *BPEL_path* to name of WS-BPEL example
  * Set *Annotation_Path* to name of Annotation file.
* Two WS-BPEL examples are included with the source code to use PASO. They are: 
  *   *Loan request* 
  *   *Shipping Service* 
* You may also need to add  *FSharp.Charting*, *FSharp.Core*, *FSharp.Data* etc in the *References* depending upon your system configuration.


