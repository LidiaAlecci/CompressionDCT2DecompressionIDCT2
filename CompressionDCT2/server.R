#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("../Script.R")

library(shiny)
library(plot.matrix)
library(bmp)
library(pixmap)

createTableBlock <- function(f,d){
    m = matrix(FALSE, ncol=f, nrow=f)
    for(row in 0:f-1){
        for(col in 0:f-1){
            if(col+row<d){
                m[row+1,col+1]=TRUE
            }
        }
    }
    return(m)
}

createOldImage <- function(image){
    if(!is.null(image)){
        path=as.character(image[4][1])
        r <- read.bmp(path)
        pr=pixmapGrey(r, cellres=1)
        return(pr)
    }
    return(0)
}

createNewImage <- function(image, f, d){
    if(!is.null(image)){
        path=as.character(image[4][1])
        r <- read.bmp(path)
        pr=pixmapIndexed(r,cellres=1)
        ncol_mImage=pr@size[2]
        nrow_mImage=pr@size[1]
        mImage=pr@index
        ncol_block=as.integer(as.numeric(ncol_mImage/f))
        nrow_block=as.integer(as.numeric(nrow_mImage/f))
        if(d==0){
            return(pixmapGrey(matrix(0, ncol = ncol_mImage, nrow = nrow_mImage), 
                              cellres=1))
        }
        for(i in 1:nrow_block){
            for(j in 1:ncol_block){
                nrow1=(i-1)*f+1
                ncol1=(i-1)*f+f
                nrow2=(j-1)*f+1
                ncol2=(j-1)*f+f    
                #DCT2
                mImage[nrow1:ncol1,nrow2:ncol2]=
                    myDCT2(mImage[nrow1:ncol1,nrow2:ncol2])
                for(k in 1:f){
                    for(l in 1:f){
                        row=((i-1)*f+k)
                        col=((j-1)*f+l)
                        if(k+l>=d+2){
                            mImage[row,col]=0
                        }
                    }
                }
                #IDCT2
                mImage[nrow1:ncol1,nrow2:ncol2]=
                    myIDCT2(mImage[nrow1:ncol1,nrow2:ncol2])
                #Round ff to the nearest integer
                for(k in 1:f){
                    for(l in 1:f){
                        row=((i-1)*f+k)
                        col=((j-1)*f+l)
                        mImage[row,col]=round(mImage[row,col],digits=0)
                        if(mImage[row,col]<0){
                            mImage[row,col]=0
                        }else if(mImage[row,col]>255){
                            mImage[row,col]=255
                        }
                    }
                }
            }
        }
        #Remaining pixels at 0
        mImage=mImage[1:(nrow_block*f), 1:(ncol_block*f)]
        #Create the new image
        npr=pixmapGrey(mImage, cellres=1)
        return(npr)
    }
    return(0)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    ######### Check d value ########
    observeEvent(input$d, { 
        updateNumericInput(session,"d", value = ({ 
            if(!(is.null(input$d) || is.na(input$d))){
                if(input$d < 0){
                    0 
                }else if(input$d > (2*input$f)-2){
                    2*input$f-2
                } else{
                    return (isolate(input$d))
                } 
            }
        })
        )
    })
    output$tablePlot <- renderPlot({
        plot(createTableBlock(input$f, input$d), main="TableBlock", key=NULL, xlab='', ylab='')
    })
    output$oldImage <- renderPlot({
        plot(createOldImage(input$image), ann=FALSE)
    })
    output$newImage <- renderPlot({
        plot(createNewImage(input$image, input$f, input$d), ann=FALSE)
    })
})
