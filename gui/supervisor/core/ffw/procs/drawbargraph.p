    
  /*--------------------------------------------------------------------
    Copyright (c) 1998 By Innovative Client Servers. All rights reserved.
    This program is freeware provided to the FreeFramework and others
    for general distribution, free of charge without warranty of
    any kind so long as this copyright notice and explanatory text is
    left intact.  Use at your own risk.

    File Name : DrawBarGraph.p - Create Bar Graph Gif

    Author    : Geoff Crawford
                Innov8 Computer Software, LLC
                geoff@innov8cs.com

    Purpose   : This program uses the freeware utility "fly" to create a GIF
                formated image of a bar graph.  An output file in the Fly
                language is created based on the user passed values.  A standard
                set of colors is used for each bar.  Bars are automatically
                scaled based on the size of the requested image.  Margins are
                left to equal the size of one bar.

    Input     : GraphFileName - Operating system file name for the GIF file to be created
                GraphTitle - Title displayed at the top of the graph
                AxisTitle - Title display along the side of the graph
                            (displayed bottom to top, vertically)
                DataList - Comma separated list of the data values
                LabelList - Comma separated list of labels corresponding
                            to the list of data
                GraphHeight - Integer value for the Height of the GIF
                GraphWidth - Integer value for the Width of the GIF
				
    Output    : NONE

	Constants : {&FLYPATH} = fully qualified path to fly on your machine.
    Revisions : 2/9/00 GC Added Titles and Labels
                2/15/00 GC Graph Height/Width reversed in Fly
                3/2/00 GC More colors and make scaling factor static
                7/29/00 Mike Brennan: There could be more bars then colors,
                        so use MOD to cycle the color list
				7/30/00 S.E. Southwell: Defined ColorNumber, added preprocessor for fly path.
  --------------------------------------------------------------------*/

/*  CHANGE THIS LINE: */
&SCOPED-DEFINE FLYPATH C:\fly\fly.exe
/*---------------------*/

PROCEDURE DrawBarGraph.

DEFINE INPUT PARAMETER GraphFileName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER GraphTitle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER AxisTitle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER DataList AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER LabelList AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER GraphHeight AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER GraphWidth AS INTEGER NO-UNDO.

DEFINE VARIABLE ColorNumber AS INTEGER NO-UNDO.
DEFINE VARIABLE BarWidth AS INTEGER NO-UNDO.
DEFINE VARIABLE MaxHeight AS DECIMAL DECIMALS 8 NO-UNDO.
DEFINE VARIABLE MaxValue AS DECIMAL DECIMALS 8 NO-UNDO.
DEFINE VARIABLE BaseAxis AS DECIMAL DECIMALS 8 NO-UNDO.
DEFINE VARIABLE ScalingFactor AS DECIMAL DECIMALS 8 NO-UNDO.
DEFINE VARIABLE NoOfBars AS INTEGER NO-UNDO.
DEFINE VARIABLE Counter AS INTEGER NO-UNDO.
DEFINE VARIABLE ColorList AS CHARACTER EXTENT 3 INITIAL ["255,  0,  0,255,  0,255,255,127,  0,  0,127,255,255,127,  0,127,127,255,127,127",
                                                         "  0,255,  0,255,255,  0,255,  0,127,  0,255,127,255,127,127,  0,127,127,255,127",
                                                         "  0,  0,255,  0,255,255,255,  0,  0,127,255,255,127,  0,127,127,255,127,127,127"].


/* Use 20% of the area at the top for the title, from there use another 55% for
   the acutal bars - in other words 75% down from the top.  The number of
   bars depends on the data and is calculated for scaling later.  The width
   of a bar assumes one bar width on either side for margins.  So a bar is
   the number of data elements plus the two margins divided by the total width.
   The top height of any one bar is then the area from the bottom where we
   start all the bars (ie BaseAxis) minus the 20% we want to reserve for the
   graph's title. */

ASSIGN BaseAxis = GraphHeight * .75
       NoOfBars = NUM-ENTRIES(DataList)
       BarWidth = GraphWidth / (NoOfBars + 2)
       MaxHeight = BaseAxis - (GraphHeight * .20).

/* We now need to get the biggest data value to do the sclaing. */

DO Counter = 1 TO NoOfBars:      
   MaxValue = MAXIMUM(MaxValue,DECIMAL(ENTRY(Counter,DataList))).
END.   

ASSIGN ScalingFactor = MaxHeight / MaxValue.

/* Create the Fly language file for processing */

OUTPUT TO VALUE(GraphFileName + ".fly").

/* Create a new graph with the dimensions specified, fill the entire thing
   with a gray background.  Then use 15% of the reserve 20% for the title. */
   
PUT UNFORMATTED "new" skip
    "size " GraphWidth "," GraphHeight skip
    "fill 1,1,200,200,200" skip
    "string 0,0,0," barwidth "," BaseAxis * .15 ",medium," GraphTitle skip
    "stringup 0,0,0," barwidth / 2.0 "," BaseAxis ",medium," AxisTitle skip.

/* Create each bar and its label from the data */
    
DO Counter = 1 TO NoOfBars:


   /* The bar - its y begins at the nth Bar and ends at the nth Bar + 1.
      The x values are from the Base Axis at the bottom, to the scaled
      height at the top.  The scaling factor consists of the Max Height / Max Value.
      
      Use the pre-filled list of colors for this bar, the color should be
      cycled if there are more bars than defined colors.  Used MOD to
      calclulate the color. MB */
   
   ASSIGN ColorNumber = (Counter MOD 20) + 1.

   PUT UNFORMATTED "frect " Counter * BarWidth ","
                BaseAxis - (DECIMAL(ENTRY(Counter,DataList)) * ScalingFactor) ","
                (Counter + 1) * BarWidth ","
                BaseAxis ","
                ENTRY(ColorNumber,ColorList[1]) ","
                ENTRY(ColorNumber,ColorList[2]) ","
                ENTRY(ColorNumber,ColorList[3]) SKIP.
    PUT UNFORMATTED "string 0,0,0," (Counter * BarWidth) + (Barwidth / 2.0) "," BaseAxis * 0.95 ",small,"
                    ENTRY(Counter,DataList) SKIP.
    
    /* The label - print going up, not across, starting at the nth Bar plus a half bar
       so the label is centered in the bar.  Start 25% below the base axis. */            
    
    PUT UNFORMATTED "stringup 0,0,0," (Counter * BarWidth) + (BarWidth / 2.0) "," BaseAxis * 1.25 ",small,"
                    ENTRY(Counter,LabelList) SKIP.

END. /* each bar */    
    
OUTPUT CLOSE.

/* Command assumes fly is in your PATH */

OS-COMMAND SILENT VALUE("{&FLYPATH} -q -i " + GraphFileName + ".fly -o " + GraphFileName).

END. /* PROCEDURE */
