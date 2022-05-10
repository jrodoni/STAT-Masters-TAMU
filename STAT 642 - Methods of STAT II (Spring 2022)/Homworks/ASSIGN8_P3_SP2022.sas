ODS HTML;
ODS GRAPHICS ON;
OPTIONS LS=120 PS=55 nocenter nodate;
DATA ;
INPUT S $ I $ M $ T $ Y @@;
LABEL S='SIGNAL' I='INTERSECTION' M='MEASUREDEVICE' T='TRAFFIC';
CARDS;

P 1 PS R  61.7    P 1 PS NR 57.4    P 1 PT R  53.1    P 1 PT NR 36.5
P 2 PS R  35.8    P 2 PS NR 18.5    P 2 PT R  35.5    P 2 PT NR 15.9
S 1 PS R  20.0    S 1 PS NR 24.6    S 1 PT R  17.0    S 1 PT NR 21.0
S 2 PS R   2.7    S 2 PS NR  3.1    S 2 PT R   1.5    S 2 PT NR  1.1
F 1 PS R  35.7    F 1 PS NR 26.8    F 1 PT R  35.4    F 1 PT NR 20.7
F 2 PS R  24.3    F 2 PS NR 25.9    F 2 PT R  27.5    F 2 PT NR 23.3

PROC MIXED CL ALPHA=.05 COVTEST METHOD=TYPE3;
CLASS S M T I ;
MODEL Y = S T  S*T  M S*M T*M  S*T*M;
RANDOM I(S) T*I(S) M*I(S);
LSMEANS S|M|T/ADJUST=TUKEY;
RUN;
ods graphics off;
ods html close;

