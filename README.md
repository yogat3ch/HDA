<a href="https://travis-ci.org/yogat3ch/HDA" target="_blank"><img src="https://travis-ci.org/yogat3ch/HDA.svg?branch=master" alt="Build Status" /></a><br>
<h3>HDA</h3><br>
<p>This is a collection of R functions found useful on multiple occasions while completing the Master of Science in Health Data Analytics at Northeastern University. This repository is intended for anyone in the Health Data Analytics program to add useful functions to in order to aid future cohorts.</p>
<h5>Installation</h5>
<p><code>rpdfclown</code> is available from R-forge. It can be installed using the following call: <code>install.packages('rpdfclown', repos=c(rforge='http://r-forge.r-project.org'))</code><br>
It depends on rJava (available from CRAN), and the <a href="https://www.java.com/en/download/" target="_blank">Java runtimes for your OS</a>. When installing rJava it must be configured to point to your Java installation. The following <a href="https://stackoverflow.com/questions/27661325/unable-to-load-rjava-on-r" target="_blank">StackOverflow post</a> details how to do this.</p>
<p><code>pdftools</code> is a dependency (available via CRAN) for extractHighlights. It relies on the libpoppler C++ library. See installation details for libpoppler below:</p>
<p>On Windows and Mac the binary packages can be installed directly from CRAN: <br>
<code>install.packages("pdftools")</code><br>
Installation on Linux requires the poppler development library. On Debian/Ubuntu: <br>
<code>sudo apt-get install libpoppler-cpp-dev</code> <br>
If you want to install the package from source on Mac OS-X you need brew: <br>
<code>brew install poppler</code> <br>
On Fedora: <br>
<code>sudo yum install poppler-cpp-devel</code>
</p>
<h5>Usage</h5>
<p>Please see package documentation for in-depth usage details until this document can be updated with usage details.</p>
<h6>General</h6>
<ul>
<li><code>%n%</code> - tests if a value is length 0, NULL, or NA and returns FALSE if it is, otherwise returns TRUE.</li>
<li><code>startPkgs</code> - loads all packages supplied by the character vector silently and quickly.</li>
<li><code>unloadPkgs</code> - silently unloads all packages in the supplied character vector</li>
<li><code>unloadAllPackages</code> - unloads all but the base packages.</li>
<li><code>Mode</code> - computes the mode of a numeric vector</li>
<li><code>findna</code> - Report summary of NA values and their Indices</li>
<li><code>find_peaks</code> - Find Peaks in a timeseries</li>
</ul>
<h6>APA</h6>
<ul>
<li><code>p.txt</code> - Formats a p-value as an APA style character vector.</li>
<li><code>apa</code> - Turn statistical test output into APA style citations.</li>
</ul>
<h6>Assumption Evaluation</h6>
<ul>
<li><code>visEDA</code> - A function for visual exploratory data analysis.</li>
<li><code>homoVariance</code> - This test performs three of the most common homogeneity of variance tests.</li>
<li><code>testTrans</code> - Test mathematical transformations (exp, log, square, cube) of predictor variables.</li>
</ul>
<h6>Themes</h6>
<ul>
	<li><code>theme_blue</code> - A dark blue theme.</li>
	<li><code>ggColor</code> - Generate hexadecimal values from R's default color wheel.</li>
</ul>
<h6>Research</h6>
<ul>
	<li><code>extractHighlights</code> - A function for extracting highlights made using a <a href="http://www.docear.org/support/user-manual/#pdf_editors" target="_blank">compatible PDF viewer</a> from withn a PDF. Also allows formatting of highlights using specific markup explained in further depth in the function documentation.</li>
</ul>
<h5>Issues</h5>
<p>Bugs are inevitable. If you find a bug, please <a href="https://github.com/yogat3ch/HDA/issues" target="_blank">document the issue</a> with a <a href="https://gist.github.com/hadley/270442" target="_blank">Reproducible Example</a>.</p>
<h5>Contributing</h5>
<p>If you would like to contribute, feel free to make a pull request! If you're wondering how to find your way around a package and want to familiarize with the knowledge necessary to contribute, check out <a href="http://r-pkgs.had.co.nz/package.html" target="_blank">Hadley Wickham's introduction to Packages</a></p>
<p>The package is AGPLv3 which specifies that any code created using the functions herein must also be open-sourced and cannot be copyrighted for commercial gain.</p>

