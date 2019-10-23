
figure1b <- function(){
	df <- data.frame(
		"activation"=c(3, 6, 11, 21),
		"bs"=c(8, 16, 32, 64)
	)
	tmp <- barplot(
		df$activation,
		names.arg=df$bs, 
		las=1,
		ylim=c(0, 25),
		ylab="Activation Size (GB)",
		xlab="Batch Size",
		main="",
		cex.axis=1.2,
		cex.lab=1.2
	)
	text(c(0.7, 1.9, 3.1, 4.3), df$activation + 1, as.character(df$activation))
}

figure2 <- function(){
	df <- data.frame(
		"CL"=0:12, 
		"DATA"=c(60, 144, 72, 108, 96, 144, 88, 40, 168, 144, 84, 96, 440)
	)
	barplot(
		df$DATA, 
		names.arg=df$CL, 
		las=1, 
		ylim=c(0, 500), 
		main="", 
		ylab="Count", 
		xlab="Class",
		cex.axis=1.2,
		cex.lab=1.2
	)
}

figure3a <- function(){
	df <- data.frame(
		"bsize"=c(4, 8, 16, 32, 64),
		"throughput"=c(5.4, 6.8, 9.5, 13.1, 12.7)
	)
	barplot(
		df$throughput, 
		names.arg=df$bsize, 
		las=1, 
		ylim=c(0, 20), 
		main="", 
		ylab="Throughput (Images/sec)", 
		xlab="Batch Size",
		cex.axis=1.2,
		cex.lab=1.2
	)
}
figure3b <- function(){
	df <- data.frame(
		"bsize"=c(4, 8, 16, 32, 64),
		"memory"=c(13.3, 17.5, 30.9, 47.5, 80)
	)
	barplot(
		df$memory, 
		names.arg=df$bsize, 
		las=1, 
		ylim=c(0, 200), 
		main="", 
		ylab="Memory Consumed (GB)", 
		xlab="Batch Size",
		cex.axis=1.2,
		cex.lab=1.2
	)
}
figure3c <- function(){
	df <- data.frame(
		"bsize"=c(4, 8, 16, 32, 64),
		"throughput"=c(12.4, 16.3, 17.7, 17.7, 18.4)
	)
	barplot(
		df$throughput, 
		names.arg=df$bsize, 
		las=1, 
		ylim=c(0, 20), 
		main="", 
		ylab="Throughput (Images/sec)", 
		xlab="Batch Size",
		cex.axis=1.2,
		cex.lab=1.2
	)
}
figure3d <- function(){
	df <- data.frame(
		"bsize"=c(4, 8, 16, 32, 64),
		"memory"=c(53, 64, 91, 125, 187)
	)
	barplot(
		df$memory, 
		names.arg=df$bsize, 
		las=1, 
		ylim=c(0, 200), 
		main="", 
		ylab="Memory Consumed (GB)", 
		xlab="Batch Size",
		cex.axis=1.2,
		cex.lab=1.2
	)
}


figure5a <- function(){
	df <- data.frame(
		"epoch"=1:30,
		"loss"=c(1.85, 1.87, 1.74, 1.74, 1.76, 1.87, 1.5, 1.42, 1.7, 1.36, 1.33, 0.73, 1.1, 0.5, 0.5, 0.47, 0.39, 0.23, 0.46, 0.9, 0.16, 0.16, 0.1, 0.08, 0.06, 0.06, 0.06, 0.05, 0.05, 0.05)
	)
	plot(
		df$epoch,
		df$loss,
		ylab="Training Loss",
		xlab="Epoch",
		ylim=c(0, 2),
		type="n",
		las=1
	)
	grid()
	lines(spline(df$epoch, df$loss), col="blue", lwd=3)
}
figure5b <- function(){
	df <- data.frame(
		"epoch"=1:30,
		"top1"=c(0.3353,  0.3838,  0.367,  0.3858,  0.3442,  0.3978,  .5786,  0.5875,  0.4688,  0.5351, 0.6775,  0.7319,  0.4619,  0.6884,  0.8536,  0.8546,  0.7488,  0.9377,  0.9031,  0.8239, 0.9753,  0.9842,  0.9941,  0.9921,  0.997,  0.996,  0.998,  0.998,  1,  1),
		"top5"=c(0.821,  0.8516,  0.8655,  0.8922,  0.6983,  0.8131,  0.9327,  0.9347,  0.8853,  0.9604, 0.9743,  0.9773,  0.8813,  0.9644,  0.9862,  0.9921,  0.9941,  0.998,  0.9951,  0.999, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1)
	)
	plot(
		df$epoch,
		df$loss,
		ylab="Validation Accuracy",
		xlab="Epoch",
		ylim=c(0.3, 1),
		xlim=c(-1, 30),
		type="n",
		las=1
	)
	grid()
	lines(spline(df$epoch, df$top1), col="blue", lwd=3)
	lines(spline(df$epoch, df$top5), col="red", lwd=3)
	legend(x=20.45, y=0.38, legend=c("top-1 accuracy", "top-5 accuracy"), lwd=3, col=c("blue", "red"), bg="white")
}

figure6 <- function(){
	df <- data.frame(
		"bs"=c(1, 2, 4, 8),
		"speedup"=c(1, 1.6, 2.5, 6.6)
	)
	barplot(
		df$speedup, 
		names.arg=df$bs, 
		las=1, 
		ylim=c(0, 7), 
		main="", 
		ylab="Speeup in TTT Relative to Single Machine", 
		xlab="Number of Nodes",
		cex.axis=1.2,
		cex.lab=1.2
	)

}

figure7 <- function(n, lr){
	#figure7(n=df$n64, lr=df$n64LR)
	setwd("~/hgWD/Novartis/Novartis/Writing/mcnn/")
	df <- read.table("epochTTT.txt", sep="\t", header=T)
	par(mar=c(5, 4, 4, 4))
	plot(
		df$epoch,
		n,
		ylim=c(0,1),
		xlim=c(-1, 21),
		las=1,
		ylab="Top-1 Accuracy",
		xlab="Epoch",
		type="n"
	)
	grid()
	lines(spline(df$epoch, n), col="black", lwd=3)
	lines(spline(df$epoch, lr*30), col="darkorange", lwd=3)
	legend(x=13, y=0.9, legend=c("Accuracy", "Learning Rate"), lwd=3, col=c("black", "darkorange"), bg="white")
	axis(4, at=seq(from=0, to=1, by=0.5/3), labels=seq(from=0, to=3, by=0.5), las=1)
	mtext("LR", side=4, las=0, line=2.4)
}

figure8 <- function(){
	df <- data.frame(
		"at"=1:7,
		"nodes"=c(1, 2, 4, 8, 16, 32, 64),
		"ideal"=c(1, 2, 4, 8, 16, 32, 64),
		"speedup"=c(1, 2, 4, 7, 15, 25, 36),
		"efficiency"=c(100, 100, 100, 88, 94, 78, 56)
	)
	par(mar=c(5, 4, 4, 4))
	plot(
		df$nodes,
		df$ideal,
		type="n",
		las=1,
		ylab="Speedup",
		xlab="",
		axes=FALSE,
	)
	grid()
	lines(df$nodes, df$ideal, col="black", lwd=3)
	lines(df$nodes, df$speedup, col="darkorange", lwd=3)
	box()
	axis(1, at=seq(from=min(df$nodes), to=max(df$nodes), length.out=7), labels=df$nodes)
	mtext("Nodes", side=1, las=0, line=2.4)
	mtext("Scaling Efficiency", side=4, las=0, line=2.4)
}
figure6()






