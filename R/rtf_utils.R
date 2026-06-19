# -----------------------------------------------------------------------------
# The functions below are copied from the 'rtf' R package
# (version 0.4-14.1, https://github.com/schaffman5/rtf).
#
# Original Author: Michael E. Schaffer <mschaff@gmail.com>
# Copyright (c) Michael E. Schaffer
# License: GPL (>= 2)
#
# Only the subset of 'rtf' required by rtfNCA() is included here, to keep the
# RTF reporting capability of 'ncar' after the 'rtf' package was archived from
# CRAN. The code retains the original R.oo / R.methodsS3 object system, so
# 'ncar' Imports R.oo and R.methodsS3 (both maintained, active CRAN packages).
# The only change from the upstream source is a corrected internal font table
# (.add.font.table): Courier New uses \ffmodern and Cambria uses font slot \f5
# (fixing a \f4 slot collision with Symbol). These functions are kept internal
# (not exported) to avoid any namespace conflict should the 'rtf' package later
# be restored or installed separately.
# -----------------------------------------------------------------------------

# ---- RTF object: constructor and S3 methods --------------------------------

setConstructorS3("RTF",
	function(file="",width=8.5,height=11,omi=c(1,1,1,1),font.size=10) {
	this <- extend(Object(), "RTF",
		.rtf = .start.rtf(width,height,omi),
		.file = file,
		.font.size = font.size,
		.indent = 0,
		.page.width = width,
		.page.height = height,
		.content.width = width - omi[2] - omi[4]
	);

	this;
});

setMethodS3("done", "RTF", function(this, ...) {
	#this$.rtf <- paste(this$.rtf,.end.rtf(),sep="")
	#write(this$.rtf,this$.file)

	# write the file, but don't close it out so that we can continue to
	# add to the object and write it out again.
	write(paste(this$.rtf,.end.rtf(),sep=""),this$.file)
})

setMethodS3("addTOC", "RTF", function(this,...) {
	toc<-"{\\field\\flddirty\\fldedit{\\*\\fldinst TOC f h}{\\fldrslt Update Field (right-click in MS Word) to show Table of Contents}}\\line\\line"

	if(!is.null(this$.font.size)) {
		font.size = this$.font.size  # default
	}

	this$.rtf <- paste(this$.rtf,.start.paragraph(indent=this$.indent,font.size=font.size),sep="")
	this$.rtf <- paste(this$.rtf,toc,sep="")
	this$.rtf <- paste(this$.rtf,.end.paragraph(),sep="")
})

setMethodS3("addHeader", "RTF", function(this, title,subtitle=NULL,font.size=NULL,TOC.level=NULL,...) {
	if(is.null(font.size)) {
		font.size = this$.font.size  # default
	}

	this$.rtf <- paste(this$.rtf,.add.header(title,subtitle=subtitle,indent=this$.indent,font.size=font.size,TOC.level=TOC.level),sep="")
})

setMethodS3("addParagraph", "RTF", function(this, ...) {
	text<-paste(... , sep="")

	if(!is.null(this$.font.size)) {
		font.size = this$.font.size  # default
	}

	this$.rtf <- paste(this$.rtf,.start.paragraph(indent=this$.indent,font.size=font.size),sep="")
	this$.rtf <- paste(this$.rtf,.add.text(text),sep="")
	this$.rtf <- paste(this$.rtf,.end.paragraph(),sep="")
})

setMethodS3("addPageBreak", "RTF", function(this, width=8.5,height=11,omi=c(1,1,1,1), ...) {
	this$.rtf <- paste(this$.rtf,.add.page.break(width=width,height=height,omi=omi),sep="")
	this$.page.width = width
	this$.page.height = height
	this$.content.width = width - omi[2] - omi[4]
})

setMethodS3("addNewLine", "RTF", function(this, n=1, ...) {
	this$.rtf <- paste(this$.rtf,.add.newline(n=n,font.size=this$.font.size),sep="")
})

setMethodS3("setFontSize", "RTF", function(this, font.size, ...) {
	this$.font.size <- font.size
})

setMethodS3("addPlot", "RTF", function(this,plot.fun=plot.fun,width=3.0,height=0.3,res=300, ...) {
	if(!is.null(this$.font.size)) {
		font.size = this$.font.size  # default
	}

	tmp.file<-tempfile("temp_rtf_plot")
	this$.rtf <- paste(this$.rtf,.start.paragraph(indent=this$.indent,font.size=font.size),sep="")
	this$.rtf <- paste(this$.rtf,.rtf.plot(plot.fun=plot.fun,tmp.file=tmp.file,width=width,height=height,res=res, ...),sep="")
	this$.rtf <- paste(this$.rtf,.end.paragraph(),sep="")

	if(file.exists(tmp.file) ) {
		unlink(tmp.file)
	}
})

# ---- Internal RTF building blocks -------------------------------------------

.start.rtf<-function(width=8.5,height=11,omi=c(1,1,1,1)) {
	paste("{\\rtf1\\ansi\n\\dntblnsbdb\n\\deff",.add.font.table(),.add.paper.size(width=width,height=height),"\n",.add.page.margins(omi),"\n",.add.page.numbers(),"\n",sep="")
}

.add.font.table<-function() {
	fonts<-character()
	fonts[1]<-"{\\f1\\fswiss\\fcharset0 Helvetica;}"
	fonts[2]<-"{\\f2\\ffmodern\\charset0\\fprg2 Courier New;}"
	fonts[3]<-"{\\f3\\ffswiss\\charset0\\fprg2 Arial;}"
	fonts[4]<-"{\\f4\\fftech\\charset0\\fprg2 Symbol;}"
	fonts[5]<-"{\\f5\\ffroman\\charset0\\fprg2 Cambria;}"

	paste("{\\fonttbl",paste(fonts,collapse="\n"),"}",sep="\n")
}

.add.page.numbers<-function() {
	paste("\\titlepg\\headery720\\footery720","{\\footer {\\pard\\qc\\fi0\\li0 \\f2\\fs20 \\field{\\fldinst{page}} \\par}}",sep="\n")
}

.add.paper.size<-function(width=8.5,height=11) {
	paste("\\paperw",round(width*1440,0),"\\paperh",round(height*1440,0),"\\widowctrl\\ftnbj\\fet0\\sectd",if(width>height){"\\lndscpsxn"} else {""},"\\linex0",sep="")
}

.add.page.margins<-function(margins=c(1,1,1,1)) {
	paste("\\margl",round(margins[2]*1440,0),"\\margr",round(margins[4]*1440,0),"\\margt",margins[3]*1440,"\\margb",margins[1]*1440,sep="")
}

.add.header<-function(title,subtitle=NULL,indent=0,font.size=10,TOC.level=NULL) {
	if(is.null(subtitle)) {
		paste("{\\pard\\fi0\\li",indent,"\\f2\\fs",font.size*2,"\\b",.get.TOC.level(TOC.level)," ",.convert(title),"\\b0\\line\\par}\n",sep="")
	} else {
		paste("{\\pard\\fi0\\li",indent,"\\f2\\fs",font.size*2,"\\b",.get.TOC.level(TOC.level)," ",.convert(title),"\\b0\\par}\n{\\pard\\fi0\\f2\\fs",font.size*2," ",.convert(subtitle),"\\line\\par}\n",sep="")
	}
}

.get.TOC.level<-function(section.level) {
	ret<-""

	if(!is.null(section.level)) {
		ret<-paste("\\s",section.level,sep="")
	}

	ret
}

.start.paragraph<-function(indent=0,font.size=10) {
	paste("{\\pard\\fi0\\li",indent,"\\f2\\fs",font.size*2,"\n",sep="")
}

.add.text<-function(x) {
	paste(.convert(x),sep="")
}

.end.paragraph<-function() {
	paste("\\par}\n",sep="")
}

.end.rtf<-function() {
	paste("}",sep="")
}

.add.page.break<-function(width=8.5,height=11,omi=c(1,1,1,1)) {
	#	"\\pard {\\f1 \\sect } \\sectd \\lndscpsxn\\pgwsxn16840\\pghsxn11907\\left\\widctlpar\\fi0\\f2\\fs18 \\par"
	# previous: "\\pard {\\f1 \\column }\\left\\widctlpar\\fi0\\f2\\fs18 \\par"
	paste("\\pard{\\f1\\sect}\\sectd",.add.paper.size(width=width,height=height),.add.page.margins(omi),"\\left\\widctlpar\\fi0\\f2\\fs18",sep="")
}

.convert<-function(x) {
	# http://www.ssec.wisc.edu/~tomw/java/unicode.html
	#x<-gsubfn("\\u(\\d+)", .hex2dec, x, engine="R")     # format UTF-8 characters from hex to dec
	#x<-gsub("\\u(\\d+)","\\\\u\\1\\\\3",x)  # format UTF-8 characters from hex to dec

	x<-gsub("\\n"," \\\\line ",x)         # convert new line to RTF \line
	#x<-gsub("\\t"," \\\\tab ",x)         # convert tab to RTF \tab
	x<-gsub("<=","\\\\u8804\\\\3",x)      # convert <= to RTF symbol
	x<-gsub(">=","\\\\u8805\\\\3",x)      # convert >= to RTF symbol

# 	x<-gsub(":delta:","\\\\u0916\\\\3",x) # convert :delta: to uppercase Greek delta
#
# 	x<-gsub("&alpha;","\\\\u0945\\\\3",x) # convert &alpha; to lowercase Greek alpha
# 	x<-gsub("&beta;","\\\\u0946\\\\3",x)  # convert &beta; to lowercase Greek beta
# 	x<-gsub("&gamma;","\\\\u0947\\\\3",x) # convert &gamma; to lowercase Greek gamma
# 	x<-gsub("&delta;","\\\\u0948\\\\3",x) # convert &delta; to lowercase Greek delta
# 	x<-gsub("&epsilon;","\\\\u0949\\\\3",x) # convert &epsilon; to lowercase Greek epsilon
# 	x<-gsub("&theta;","\\\\u0952\\\\3",x) # convert &theta; to lowercase Greek theta
# 	x<-gsub("&kappa;","\\\\u0954\\\\3",x) # convert &kappa; to lowercase Greek kappa
# 	x<-gsub("&lambda;","\\\\u0955\\\\3",x) # convert &lambda; to lowercase Greek lambda
# 	x<-gsub("&mu;","\\\\u0956\\\\3",x)    # convert &mu; to lowercase Greek lambda
#
# 	x<-gsub("&Alpha;","\\\\u0913\\\\3",x) # convert &Alpha; to uppercase Greek alpha
# 	x<-gsub("&Beta;","\\\\u0914\\\\3",x)  # convert &Beta; to uppercase Greek beta
# 	x<-gsub("&Gamma;","\\\\u0915\\\\3",x) # convert &Gamma; to uppercase Greek gamma
# 	x<-gsub("&Delta;","\\\\u0916\\\\3",x) # convert &Delta; to uppercase Greek delta
# 	x<-gsub("&Epsilon;","\\\\u0917\\\\3",x) # convert &Epsilon; to uppercase Greek epsilon
# 	x<-gsub("&Theta;","\\\\u0920\\\\3",x) # convert &Theta; to uppercase Greek theta
# 	x<-gsub("&Kappa;","\\\\u0922\\\\3",x) # convert &Kappa; to lowercase Greek kappa
# 	x<-gsub("&Lambda;","\\\\u0923\\\\3",x) # convert &Lambda; to lowercase Greek lambda
# 	x<-gsub("&Mu;","\\\\u0924\\\\3",x)     # convert &Mu; to lowercase Greek lambda

	# convert HTML characters
	x<-gsub("&gt;",">",x)
	x<-gsub("&lt;","<",x)

	# convert uppercase and lowercase Greek letters
	x<-gsub("&Alpha;","\\\\u0913\\\\3",x)
	x<-gsub("&Beta;","\\\\u0914\\\\3",x)
	x<-gsub("&Gamma;","\\\\u0915\\\\3",x)
	x<-gsub("&Delta;","\\\\u0916\\\\3",x)
	x<-gsub("&Epsilon;","\\\\u0917\\\\3",x)
	x<-gsub("&Zeta;","\\\\u0918\\\\3",x)
	x<-gsub("&Eta;","\\\\u0919\\\\3",x)
	x<-gsub("&Theta;","\\\\u0920\\\\3",x)
	x<-gsub("&Iota;","\\\\u0921\\\\3",x)
	x<-gsub("&Kappa;","\\\\u0922\\\\3",x)
	x<-gsub("&Lambda;","\\\\u0923\\\\3",x)
	x<-gsub("&Mu;","\\\\u0924\\\\3",x)
	x<-gsub("&Nu;","\\\\u0925\\\\3",x)
	x<-gsub("&Xi;","\\\\u0926\\\\3",x)
	x<-gsub("&Omicron;","\\\\u0927\\\\3",x)
	x<-gsub("&Pi;","\\\\u0928\\\\3",x)
	x<-gsub("&Rho;","\\\\u0929\\\\3",x)
	x<-gsub("&Sigma;","\\\\u0931\\\\3",x)
	x<-gsub("&Tau;","\\\\u0932\\\\3",x)
	x<-gsub("&Upsilon;","\\\\u0933\\\\3",x)
	x<-gsub("&Phi;","\\\\u0934\\\\3",x)
	x<-gsub("&Chi;","\\\\u0935\\\\3",x)
	x<-gsub("&Psi;","\\\\u0936\\\\3",x)
	x<-gsub("&Omega;","\\\\u0937\\\\3",x)
	x<-gsub("&alpha;","\\\\u0945\\\\3",x)
	x<-gsub("&beta;","\\\\u0946\\\\3",x)
	x<-gsub("&gamma;","\\\\u0947\\\\3",x)
	x<-gsub("&delta;","\\\\u0948\\\\3",x)
	x<-gsub("&epsilon;","\\\\u0949\\\\3",x)
	x<-gsub("&zeta;","\\\\u0950\\\\3",x)
	x<-gsub("&eta;","\\\\u0951\\\\3",x)
	x<-gsub("&theta;","\\\\u0952\\\\3",x)
	x<-gsub("&iota;","\\\\u0953\\\\3",x)
	x<-gsub("&kappa;","\\\\u0954\\\\3",x)
	x<-gsub("&lambda;","\\\\u0955\\\\3",x)
	x<-gsub("&mu;","\\\\u0956\\\\3",x)
	x<-gsub("&nu;","\\\\u0957\\\\3",x)
	x<-gsub("&xi;","\\\\u0958\\\\3",x)
	x<-gsub("&omicron;","\\\\u0959\\\\3",x)
	x<-gsub("&pi;","\\\\u0960\\\\3",x)
	x<-gsub("&rho;","\\\\u0961\\\\3",x)
	x<-gsub("&sigmaf;","\\\\u0962\\\\3",x)
	x<-gsub("&sigma;","\\\\u0963\\\\3",x)
	x<-gsub("&tau;","\\\\u0964\\\\3",x)
	x<-gsub("&upsilon;","\\\\u0965\\\\3",x)
	x<-gsub("&phi;","\\\\u0966\\\\3",x)
	x<-gsub("&chi;","\\\\u0967\\\\3",x)
	x<-gsub("&psi;","\\\\u0968\\\\3",x)
	x<-gsub("&omega;","\\\\u0969\\\\3",x)

	x<-gsub("TRUE","Yes",x)
	x<-gsub("FALSE","No",x)
	x
}

.add.newline<-function(n=NULL, font.size=10) {
	# return("\\line ")

	ret<-paste("{\\pard\\fi0\\f2\\fs",(font.size*2),sep="")

	if(is.null(n)) {
		if(n>=2) {
			ret<-paste(ret,paste(rep("\\line",n),"\n",collapse="",sep=""),sep="")
		}
	}
	paste(ret,"\\par}",sep="")
}

.add.png<-function(file,width=3,height=3,verbose=FALSE) {
	# return a hexadecimal version of a file
	max.bytes<-50000000  # maximum file size in bytes (~50MB)
	dat<-readBin(file, what="raw", size=1, signed=TRUE, endian="little",n=max.bytes);
	if(verbose) {
		cat(paste(length(dat),"bytes read\n"))
	}
	paste("{\\rtf1\\ansi\\deff0{\\pict\\pngblip\\picwgoal",round(width*1440),"\\pichgoal",round(height*1440)," ",paste(dat,collapse=""),"}}",sep="")
	# paste("{\\rtf1\\ansi\\deff0{\\pict\\pngblip\\picwgoal",round(width*1440),"\\pichgoal",round(height*1440)," \n",.chunk.vector(dat),"}}",sep="")
}

.rtf.plot<-function(plot.fun,tmp.file="temp.png",width=3.0,height=0.3,res=300, ...) {
	width.px<-round(width*res)
	height.px<-round(height*res)
	#png(tmp.file,width=width.px,height=height.px,units="px",pointsize=8,bg = "white",res=res)
	png(tmp.file,width=width.px,height=height.px,units="px",pointsize=8,bg = "transparent",res=res)
	plot.fun(...)
	dev.off()
	.add.png(tmp.file,width=width,height=height)
}

