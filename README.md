
Overview
--------

Visualising and analysing animal movement patterns is essential to many behavioural studies. While commercial options exist for analysing animal movement via video, the cost of these is often prohibitive. To meet the need for an efficient and cost-effective video tracking and analysis tool, we have developed the 'pathtrackr' package. The 'pathtrackr' package allows for an automated and consolidated workflow, from video input to statistical output of an animal's movement. The tracking functions work across a variety of visual contexts, including heterogeneous backgrounds and variable lighting, and can deal with small amounts of localised background movement. We also include diagnostic tools in the package for troubleshooting. Future updates will include the ability to track multiple animals simultaneously, extending the functionality to track animal interactions.

Installation
------------

#### pathtrackr

You can install *pathtrackr* directly from within R using the *install\_github()* function from the *[devtools](https://github.com/hadley/devtools#readme)* package:

``` r
install_github("aharmer/pathtrackr")
```

Alternatively, you can manually download and install the source package from the [pathtrackr](https://aharmer.github.io/pathtrackr/) webpage.

#### FFmpeg

The *pathtrackr* package includes functions for handling video files; *splitVideo()*, which compresses and splits a video into individual jpeg frames that are used by the tracking functions, and *makeVideo()*, which outputs an mp4 video file of the tracking behaviour performed by the tracking functions, along with summary plots for each frame. These functions require [FFmpeg](https://ffmpeg.org) to be installed on your machine, which R calls via the *system()* function.

If you do not already have FFmpeg installed, these links provide excellent instructions:

[Install FFmpeg on Mac](http://www.idiotinside.com/2016/05/01/ffmpeg-mac-os-x/)     [Install FFmpeg on Windows](http://www.wikihow.com/Install-FFmpeg-on-Windows)

How to use pathtrackr
---------------------

Using *pathtrackr* is very straightforward. Below are simple steps to process and analsyse your animal movement videos.

#### 1. Extract jpegs from video

``` r
splitVideo(filepath, fps, xpix = 320, ypix = -1)
```

Using the function *splitVideo*, specify the: \* file path of your video \* the number of frames per second to extract. If fps is equal to the recording frame rate, all frames will be extracted. For fast moving animals, it is recommended to use a higher frame rate \* size (in pixels) of the extracted jpegs. The tracking functions here do not require high resolution imagery, so we recommend using the default values for faster processing time. A value of -1 for ypix will maintain the aspect ratio.

The still frames will be saved in a new directory with the same name as the video file.

#### 2. Track your animal's movement

``` r
trackPath(dirpath, xarena, yarena, fps, box = 2, contrast = 0.5)
```

Next, use the *trackPath()* function and assign to a new object. Specify the directory containing your extracted jpegs, the dimensions of your arena in mm, and the frame rate at which you extracted still frames from the video. The function will work if these values are incorrectly specified, however, they are used in distance and velocity calculations later, so will return incorrect values.

After running the function, you will be prompted to define the animal in the first frame of the video. Click once to select one corner of the tracking box, and click once more to select the opposing corner.

<<<<<<< HEAD
<img src="https://cloud.githubusercontent.com/assets/10540385/16355521/e27c4cd4-3b0c-11e6-9f2f-2e8f841ef308.jpg" width="600" />  
=======
<img src="https://cloud.githubusercontent.com/assets/10540385/16355521/e27c4cd4-3b0c-11e6-9f2f-2e8f841ef308.jpg" width="600" />

<!-- ![animal_selection](https://cloud.githubusercontent.com/assets/10540385/16355521/e27c4cd4-3b0c-11e6-9f2f-2e8f841ef308.jpg) -->
 
>>>>>>> 378a1c12a219468b72c5e57bc279bbfddc6c0847

You will then be prompted to define the edges of the arena. Again, select the opposing corners on the image that mark the edges of the arena.

<img src="https://cloud.githubusercontent.com/assets/10540385/16355522/e81f8f3e-3b0c-11e6-9c53-53bd9eba0b70.jpg" width="600" />

The function will now begin tracking your animal across frames and return a list with a range of useful information, including a matrix of the xy co-ordinates of the animal in each frame, a matrix of movement data such as distances, velocities and bearings bewteen each frame, and some summary data. Each of these elements can be retrieved by referencing the appropriate element in the returned list.

#### 3. Make some pretty plots

``` r
plotPath(path.list)
```

The *plotPath()* function will plot the animal's path across the arena with density clouds representing the relative time spent in a given location. The only input required is the names list generated previously.

<img src="https://cloud.githubusercontent.com/assets/10540385/16355859/837b3d7e-3b18-11e6-9884-9e042ed8f0ce.jpg" width="600" />

   

``` r
 plotSummary(path.list)
```

The *plotSummary()* function returns a four-paneled plot summarising the the animal's path, i.e. cumulative distance and velocity over time, and absolute and relative bearings across frames.

<img src="https://cloud.githubusercontent.com/assets/10540385/16355860/8f92255a-3b18-11e6-8b48-d8961536dad6.jpg" width="600" />

   

``` r
makeVideo(dirpath, xarena, yarena, fps, box = 2, contrast = 0.5)
```

You can also produce a video output of the animal's movement, along with summary plots, using the *makeVideo()* function. The function variables are the same as the main tracking function above, but instead of returning a list of data, an mp4 file will be saved in the same directory as the original video.

<img src="https://cloud.githubusercontent.com/assets/10540385/16360174/3a8ab69c-3ba6-11e6-82b8-e72363e2bf31.gif" width="600" />

   

#### 4. Troubleshooting

If your animal is not being tracked properly, use the *diagnosticPDF()* function to view the tracking behaviour in each frame. Function variables are again the same as the main tracking function but in addition to returning a list of data, a pdf file will be saved with a page of plots for each frame. These can be used to visually compare the data with what is actually happening during tracking.
