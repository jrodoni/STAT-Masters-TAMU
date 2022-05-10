
* REPEAT_4METHODS.SAS;
*Example is from Crowder and Hand, "Repeated Measures"
Three levels of Vitamin E supplements: Control(C), Low(L), and High(H)
are to be evaluated. The experimental units are guinea pigs. Five pigs
are randomly assigned to each of the three treatments. The weight of the
pigs were recorded after 1, 2, 3, 4, 5, and 6 weeks after receiving the
treatment;

option ls=90 ps=55 nocenter nodate; 
ods rtf file='u:/meth2/handouts/4methods.rtf';
title 'ANALYSIS OF REPEATED MEASURES DESIGN';
data RAW; 
INPUT ANM  TRT $ @;
DO WK = 1 TO 6;    
INPUT Y @; OUTPUT; END;
LABEL WK = 'WEEK' TRT = 'LEVEL OF VITAMIN E' Y='WEIGHT';
cards;                                                             
 1 C 455 460 510 504 436 466
 2 C 467 565 610 596 542 587 
 3 C 445 530 580 597 582 619
 4 C 485 542 594 583 611 612
 5 C 480 500 550 528 562 576
 1 L 514 560 565 524 552 597
 2 L 440 480 536 484 567 569
 3 L 495 570 569 585 576 677
 4 L 520 590 610 637 671 702
 5 L 503 555 591 605 649 675
 1 H 496 560 622 622 632 670
 2 H 498 540 589 557 568 609
 3 H 478 510 568 555 576 605
 4 H 545 565 580 601 633 649
 5 H 472 498 540 524 532 583
RUN;
*PROC PRINT DATA=RAW;
*VAR ANM TRT WK Y ;
*RUN;
TITLE2 'ANALYSIS AS A SPLIT PLOT DESIGN';
PROC MIXED DATA=RAW;
 CLASS ANM TRT WK;
 MODEL Y = TRT WK TRT*WK;
 Random ANM(TRT);
 CONTRAST 'T1 VS T2 WK=1' 
TRT 1 -1 
TRT*WK 1 0 0 0 0 0 -1 0 0 0 0 0;
LSMEANS TRT WK/adjust=Tukey;
RUN;

RUN;TITLE2 'ANALYSIS AS A REPEATED MEASURES DESIGN-PROC MIXED TYPE=UN';
PROC MIXED DATA=RAW;
 CLASS ANM TRT WK;
 MODEL Y = TRT WK TRT*WK;
 REPEATED WK/SUB=ANM(TRT) TYPE=UN R RCorr;
  CONTRAST 'T1 VS T2 WK=1' 
TRT 1 -1 
TRT*WK 1 0 0 0 0 0 -1 0 0 0 0 0;
LSMEANS TRT WK /adjust=Tukey;
RUN;
TITLE2 'ANALYSIS AS A REPEATED MEASURES DESIGN-PROC MIXED TYPE=CS';
PROC MIXED DATA=RAW;
 CLASS ANM TRT WK;
 MODEL Y = TRT WK TRT*WK;
 REPEATED WK/SUB=ANM(TRT) TYPE=CS R RCorr;
 CONTRAST 'T1 VS T2 WK=1' 
TRT 1 -1 
TRT*WK 1 0 0 0 0 0  -1 0 0 0 0 0;
  LSMEANS TRT WK/adjust=Tukey;
RUN;
TITLE2 'ANALYSIS AS A REPEATED MEASURES DESIGN-PROC MIXED TYPE=AR(1)';
PROC MIXED DATA=RAW;
 CLASS ANM TRT WK;
 MODEL Y = TRT WK TRT*WK;
 REPEATED WK/SUB=ANM(TRT) TYPE=AR(1) R RCorr;
 CONTRAST 'T1 VS T2 WK=1' 
TRT 1 -1 
TRT*WK 1 0 0 0 0 0  -1 0 0 0 0 0;
  LSMEANS TRT WK /adjust=Tukey;
RUN;
ods rtf close;
