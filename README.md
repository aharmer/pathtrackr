
Overview
--------

Visualising and analysing animal movement patterns is essential to many behavioural studies. While commercial options exist for analysing animal movement via video, the cost of these is often prohibitive. To meet the need for an efficient and cost-effective video tracking and analysis tool, we have developed the 'pathtrackr' package. The 'pathtrackr' package allows for an automated and consolidated workflow, from video input to statistical output of an animal's movement. The tracking functions work across a variety of visual contexts, including heterogeneous backgrounds and variable lighting, and can deal with small amounts of localised background movement. We also include diagnostic tools in the package for troubleshooting. Planned future updates will include the ability to track multiple animals simultaneously, extending the functionality to track animal interactions.

Updates
-------

In version 1.1 of *pathtrackr* we have updated the *trackPath()* function to now be able to handle not only hetergenous backgrounds, but instances where the animal is at times lighter and at times darker than the background. This is demonstrated in an extreme example below, where a brown spider is moving across a black and white checkerboard pattern.

<img src="https://user-images.githubusercontent.com/10540385/30011056-996a0850-9188-11e7-803f-980cc3d8a154.gif" width="600" style="display: block; margin: auto;" />

Installation
------------

#### pathtrackr

You can install *pathtrackr* directly within R using the *install\_github()* function from the [devtools](https://www.rstudio.com/products/rpackages/devtools/) package:

``` r
install_github("aharmer/pathtrackr")
```

Depending on your setup, you may also need to install *Rtools* first. If you need *Rtools* you will get an error message during *pathtrackr* installation. Just install *Rtools* then reinstall *pathtrackr*.

Alternatively, you can manually download and install the source package from the [pathtrackr](https://aharmer.github.io/pathtrackr/) webpage.

If you manually install *pathtrackr*, the following packages are also required: *ggplot2, plyr, pbapply, abind, cluster, gridExtra, jpeg, marmap, EBImage, imager, viridis, raster*.

#### FFmpeg

The *pathtrackr* package includes functions for handling video files; *splitVideo()*, which compresses and splits a video into individual jpeg frames that are used by the tracking functions, and *makeVideo()*, which outputs an mp4 video file of the tracking behaviour performed by the tracking functions, along with summary plots for each frame. These functions require [FFmpeg](https://ffmpeg.org) to be installed on your machine, which R calls via the *system()* function.

If you do not already have FFmpeg installed, these links provide excellent instructions:

[Install FFmpeg on Mac](http://www.idiotinside.com/2016/05/01/ffmpeg-mac-os-x/)  
[Install FFmpeg on Windows](http://www.wikihow.com/Install-FFmpeg-on-Windows)

How to use pathtrackr
---------------------

Using *pathtrackr* is very straightforward. Below are simple steps to process and analyse your animal movement videos.

#### Extract jpegs from video

``` r
splitVideo(filepath, fps, xpix, ypix)
```

Using the function *splitVideo()*, specify the:

-   file path of your video

-   the number of frames per second to extract. If fps is equal to the recording frame rate, all frames will be extracted. For fast moving animals, it is recommended to use a higher frame rate

-   size (in pixels) of the extracted jpegs. The tracking functions here do not require high resolution imagery, so we recommend using the default values for faster processing time. A value of -1 for ypix will maintain the aspect ratio.

The still frames will be saved in a new directory with the same name as the video file.

#### Track your animal's movement

``` r
path.list = trackPath(dirpath, xarena, yarena, fps = 30, box = 1, jitter.damp = 0.9)
```

Next, use the *trackPath()* function and assign the results to a new object. Specify the newly created directory containing your extracted jpegs, the dimensions of your arena in mm, and the frame rate at which you extracted still frames from the video. By default this is set 30 fps, but you will need to adjust *fps* according to the frame rate at which you extracted stills from your video. The *trackPath* function will work if arena size and fps values are incorrectly specified, however, they are used in distance and velocity calculations later, so will return incorrect values. You can also specify the size of the tracking box. By default this is set to 1, and is a good place to start. If your animal is particularly fast moving, a larger box size may be more appropriate. Finally, if the animal's path is very noisy (i.e. jittery) you may wish to increase the jitter damping. This is set to 0.9 by default. A value of 1 indicates no jitter damping, 0.5 indicates extreme damping and is unlikely to be useful. Too much jitter in the animal's path will result in an overestimate of the path length. The default will usually produce good results unless your animal is particularly elongated.

After running the function and the still frames have been loaded, you will be prompted to define the boundaries of the arena. Click once on the top left corner of your arena, followed by clicking once on the bottom right corner of your arena, to define the opposing corners of the entire arena.

<img src="https://user-images.githubusercontent.com/10540385/30007883-b9e44756-916c-11e7-8fd7-36add552eaaa.png" width="600" style="display: block; margin: auto;" />  

You will then be prompted to define the animal in the first frame of the video. Imagine a rectangle that defines the minimum region of your arena that contains your whole animal. Click once to define the top left corner of this rectangle, followed by clicking once to define the bottom right corner of this rectangle.

<img src="https://user-images.githubusercontent.com/10540385/30007903-dfb1619e-916c-11e7-8225-0ac1507d86ec.png" width="600" style="display: block; margin: auto;" />

The function will now define a background reference image and subtract it from each frame, this may take some time depending on your video resolution and number of frames. The function will now begin tracking your animal across frames and return a list with a range of useful information, including a matrix of the xy co-ordinates of the animal in each frame, a matrix of movement data such as distances, velocities and bearings between each frame, and some summary data. Each of these elements can be retrieved by referencing the appropriate element in the returned list.

#### Make some pretty plots

``` r
plotPath(path.list)
```

The *plotPath()* function will plot the animal's path across the arena with density clouds representing the relative time spent in a given location. The only input required is the *path.list* object generated by *trackPath()*.

<img src="https://user-images.githubusercontent.com/10540385/30009277-327be3aa-917b-11e7-941e-8d43eb60ae52.jpeg" width="600" style="display: block; margin: auto;" />

   

The *plotSummary()* function returns a four-panelled plot summarising the the animal's path, i.e. cumulative distance and velocity over time, and absolute and relative bearings across frames. The *pathSummary()* function will return summary data in a table format. The only input required for these functions is the *path.list* object generated by *trackPath()*.

``` r
 plotSummary(path.list)
 pathSummary(path.list)
```

<img src="https://user-images.githubusercontent.com/10540385/30009666-452d8262-917e-11e7-9d3d-85b8e23e25cd.jpeg" width="600" style="display: block; margin: auto;" />

   

You can also produce a video output of the animal's movement, along with summary plots, using the *makeVideo()* function. The function variables are the same as the main tracking function above, but instead of returning a list of data, an mp4 file will be saved in the same directory as the original video. **Note:** Tracking will take significantly longer than the normal *trackPath* function.

``` r
makeVideo(dirpath, xarena, yarena, fps = 30, box = 1, jitter.damp = 0.9)
```

<img src="https://user-images.githubusercontent.com/10540385/30010347-28bb0848-9183-11e7-90d2-593f26221068.gif" width="600" style="display: block; margin: auto;" />

   

#### Troubleshooting

If your animal is not being tracked properly, use the *diagnosticPDF()* function to view the tracking behaviour in each frame. Function variables are again the same as the main tracking function but in addition to returning a list of data, a pdf file will be saved with a page of plots for each frame. These can be used to visually compare the data with what is actually happening during tracking. **Note:** Tracking will take significantly longer than the normal *trackPath* function and a very large pdf file may be produced.

``` r
path.list = diagnosticPDF(dirpath, xarena, yarena, fps = 30, box = 1, jitter.damp = 0.9)
```

Known issues and planned features
---------------------------------

Currently, *pathtrackr* is only able to track a single animal at a time. We plan to include the ability to track multiple animals simultaneously in the future, but are still tackling the issue of keeping track of individual ID when animals' paths intersect.
