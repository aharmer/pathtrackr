
Overview
--------

Visualising and analysing animal movement patterns is essential to many behavioural studies. While commercial options exist for analysing animal movement via video, the cost of these is often prohibitive. To meet the need for an efficient and cost-effective video tracking and analysis tool, we have developed the 'pathtrackr' package. The 'pathtrackr' package allows for an automated and consolidated workflow, from video input to statistical output of an animal's movement. The tracking functions work across a variety of visual contexts, including heterogeneous backgrounds and variable lighting, and can deal with small amounts of localised background movement. We also include diagnostic tools in the package for troubleshooting. Future updates will include the ability to track multiple animals simultaneously, extending the functionality to track animal interactions.

Installation
------------

#### pathtrackr

You can install *pathtrackr* directly within R using the *install\_github()* function from the [devtools](https://www.rstudio.com/products/rpackages/devtools/) package:

``` r
install_github("aharmer/pathtrackr")
```

Alternatively, you can manually download and install the source package from the [pathtrackr](https://aharmer.github.io/pathtrackr/) webpage.

#### FFmpeg

The *pathtrackr* package includes functions for handling video files; *splitVideo()*, which compresses and splits a video into individual jpeg frames that are used by the tracking functions, and *makeVideo()*, which outputs an mp4 video file of the tracking behaviour performed by the tracking functions, along with summary plots for each frame. These functions require [FFmpeg](https://ffmpeg.org) to be installed on your machine, which R calls via the *system()* function.

If you do not already have FFmpeg installed, these links provide excellent instructions:

[Install FFmpeg on Mac](http://www.idiotinside.com/2016/05/01/ffmpeg-mac-os-x/)  
[Install FFmpeg on Windows](http://www.wikihow.com/Install-FFmpeg-on-Windows)

How to use pathtrackr
---------------------

Using *pathtrackr* is very straightforward. Below are simple steps to process and analyse your animal movement videos.

#### Extract jpegs from video

Using the function *splitVideo()*, specify the:

-   file path of your video

-   the number of frames per second to extract. If fps is equal to the recording frame rate, all frames will be extracted. For fast moving animals, it is recommended to use a higher frame rate

-   size (in pixels) of the extracted jpegs. The tracking functions here do not require high resolution imagery, so we recommend using the default values for faster processing time. A value of -1 for ypix will maintain the aspect ratio.

``` r
splitVideo(filepath, fps, xpix, ypix)
```

The still frames will be saved in a new directory with the same name as the video file.

#### Track your animal's movement

Next, use the *trackPath()* function and assign to a new object. Specify the directory containing your extracted jpegs, the dimensions of your arena in mm, and the frame rate at which you extracted still frames from the video. The function will work if these values are incorrectly specified, however, they are used in distance and velocity calculations later, so will return incorrect values.

``` r
trackPath(dirpath, xarena, yarena, fps, box, contrast)
```

After running the function, you will be prompted to define the animal in the first frame of the video. Click once to select one corner of the tracking box, and click once more to select the opposing corner.

<img src="https://cloud.githubusercontent.com/assets/10540385/16403090/c31ba764-3d45-11e6-89ba-6302cb8ad3ea.png" width="600" style="display: block; margin: auto;" />  

You will then be prompted to define the edges of the arena. Again, select the opposing corners on the image that mark the edges of the arena.

<img src="https://cloud.githubusercontent.com/assets/10540385/16403105/fa317e18-3d45-11e6-8c11-ab6f01b4c958.png" width="600" style="display: block; margin: auto;" />

The function will now begin tracking your animal across frames and return a list with a range of useful information, including a matrix of the xy co-ordinates of the animal in each frame, a matrix of movement data such as distances, velocities and bearings between each frame, and some summary data. Each of these elements can be retrieved by referencing the appropriate element in the returned list.

#### Make some pretty plots

The *plotPath()* function will plot the animal's path across the arena with density clouds representing the relative time spent in a given location. The only input required is the names list generated previously.

``` r
plotPath(path.list)
```

<img src="https://cloud.githubusercontent.com/assets/10540385/16403189/c7d5cb80-3d46-11e6-816b-ad94fccfd70a.png" width="600" style="display: block; margin: auto;" />

   

The *plotSummary()* function returns a four-panelled plot summarising the the animal's path, i.e. cumulative distance and velocity over time, and absolute and relative bearings across frames.

``` r
 plotSummary(path.list)
```

<img src="https://cloud.githubusercontent.com/assets/10540385/16403223/28b30968-3d47-11e6-8d24-54650cdb15e1.png" width="600" style="display: block; margin: auto;" />

   

You can also produce a video output of the animal's movement, along with summary plots, using the *makeVideo()* function. The function variables are the same as the main tracking function above, but instead of returning a list of data, an mp4 file will be saved in the same directory as the original video.

``` r
makeVideo(dirpath, xarena, yarena, fps, box, contrast)
```

<img src="https://cloud.githubusercontent.com/assets/10540385/16403444/4c241fca-3d49-11e6-82cf-8b551faff7f1.gif" width="600" style="display: block; margin: auto;" />

   

#### Troubleshooting

If your animal is not being tracked properly, use the *diagnosticPDF()* function to view the tracking behaviour in each frame. Function variables are again the same as the main tracking function but in addition to returning a list of data, a pdf file will be saved with a page of plots for each frame. These can be used to visually compare the data with what is actually happening during tracking.

Known issues and planned features
---------------------------------

Currently, *pathtrackr* is only able to track a single animal at a time. We plan to include the ability to track multiple animals simultaneously in the near future.

While the tracking functions in *pathtrackr* are robust to jpeg noise, lighting changes and background movement, there is the requirement that the animal is always darker (or always lighter) than the background. This is necessary as to successfully track an animal, it must be distinguishable from the background. If the animal is sometimes darker and sometimes lighter, tracking will fail.
