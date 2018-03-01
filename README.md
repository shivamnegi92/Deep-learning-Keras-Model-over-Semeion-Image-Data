# Deep learning/Keras Model over Semeion Handwritten Digit Image Data

Prezi:https://prezi.com/p/r1lkzdza1ffr/

## Data Set Information: ##
<img align="right" width="100" height="100" src="https://archive.ics.uci.edu/ml/assets/MLimages/Large178.jpg">

1593 handwritten digits from around 80 persons were scanned, stretched in a rectangular box 16x16 in a gray scale of 256 values.Then each pixel of each image was scaled into a bolean (1/0) value using a fixed threshold. 
Each person wrote on a paper all the digits from 0 to 9, twice. The commitment was to write the digit the first time in the normal way (trying to write each digit accurately) and the second time in a fast way (with no accuracy). 

## Data Analysis: ##
* Implemented Keras Machine Learing algorithm to develop a deep net trained on the test data
* Trained the hyperparameters and performed model tuning by adjusting epoch and number of layers to optimize the outputs and make the performance better
* Developed the model with a performance accuracy of around 96% which classify the nuumeric image data to a resultant value


<p align="center">
  <img width="460" height="300" src="https://3qeqpr26caki16dnhd19sv6by6v-wpengine.netdna-ssl.com/wp-content/uploads/2016/05/Examples-from-the-MNIST-dataset.png">
</p>


## References: ##

The MNIST dataset is very well studied. Below are some additional resources you might like to look into.

* [The Official MNIST dataset webpage](https://archive.ics.uci.edu/ml/datasets/semeion+handwritten+digit)
* [Rodrigo Benenson’s webpage that lists state of the art results.](http://rodrigob.github.io/are_we_there_yet/build/classification_datasets_results.html#4d4e495354)
* [Kaggle competition that uses this dataset](https://www.kaggle.com/c/digit-recognizer) (check the scripts and forum sections for sample code)
* [Read-only model trained on MNIST that you can test in your browser](http://myselph.de/neuralNet.html) (very cool)

