erlang-id3
==========
A Decision tree inducer (originally based on ID3).

It handles:
   * Numeric features
   * Categoric features
   * Asynchronous branch induction (for performance)


USAGE
=====
    $ ./tr -h
    Decision tree inducer
    Version (of 2012-10-27): 0.1.1
    All rights reserved Isak Karlsson 2012+

    Example: tr -i <EXAMPLES> -y 0.66 -d 5 
	     tr -i <EXAMPLES> -y 1 -d 0 

    -y  [Percent training data] 
	 A percentage of data that is used for
	 training the model. The rest is used for validation.
	 Default: 0.66
    -d  [Maximum depth to paralellize]
	 The depth in which the model is induced in paralell.
	 Default: 5
    -ag []
	 Calculate gain in parallel even thought the MAX_DEPTH
	 is reached (-d).
    -o  []
	 Output the tree model
    -p  [N instances required to partition]
	 Stop inducing a branch (and take the majority) when
	 N is reached (for a given branch)
	 Default: 10

    For example,

    $ git clone ....
    $ cd erlang-id3/src
    $ ./tr -i ../data/bank.txt -y 0.66 -d 10 -ag -p 100 -o > model.txt
    Running:
     * File: "../data/bank.txt"
     * Split: 0.66
     * Depth: 10
     * Prune: 100
     * Gain async: true
    Took: 1.159709 
    Accuracy: 0.8792531390280398 

LICENCE
=======
BSD