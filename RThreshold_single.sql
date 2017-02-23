-----ATE Threshold table
DROP TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD;
CREATE COLUMN TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD(
	"MATERIAL_ID_INDEX" INTEGER, 
	--"ATE_INDEX" INTEGER,
	"THRESHOLD" DOUBLE,
	"RUNID" VARCHAR(50)
);

-----MATERIAL Batch lot Threshold table
DROP TABLE "PAPOC".RMODEL_PREDICTION_BL_THRESHOLD;
CREATE COLUMN TABLE "PAPOC".RMODEL_PREDICTION_BL_THRESHOLD(
	"MATERIAL_ID_INDEX" INTEGER, 
	"THRESHOLD" DOUBLE,
	"RUNID" VARCHAR(50)
);

-----single statistic data type
DROP TYPE "PAPOC".RMODEL_THRESHOLD_STA_TT;
CREATE TYPE "PAPOC".RMODEL_THRESHOLD_STA_TT AS TABLE(
"STATISTIC" DOUBLE
);

-----single threshold result type
DROP TYPE "PAPOC".RMODEL_THRESHOLD_TT;
CREATE TYPE "PAPOC".RMODEL_THRESHOLD_TT AS TABLE(
"THRESHOLD" DOUBLE
);


-----R Procedure for threshold calculate one by one
DROP PROCEDURE "PAPOC".RMODEL_USE_THRESHOLD_SINGLE_SRC;
CREATE PROCEDURE "PAPOC".RMODEL_USE_THRESHOLD_SINGLE_SRC(IN statistic "PAPOC".RMODEL_THRESHOLD_STA_TT, OUT result "PAPOC".RMODEL_THRESHOLD_TT)
LANGUAGE RLANG AS

BEGIN

  setwd("/hana/data/HT1/tmp/Rserve/pamodel/")
  
  ##Threshold function
  
  calculateOtsu <- function(allGreyValues){ 
  gTrans=allGreyValues*255  
  gTrans=round(gTrans)  
  t=tabulate(gTrans)
  sdbenchMark <- 2  
  
  ####check greyvalue volumn must >1
  if(length(gTrans)<2){
    threshold = allGreyValues*sdbenchMark
    return(threshold)
  }
  
  
  ####check balance
  
  rHist=hist(gTrans,plot=FALSE)  
  mHist=mean(gTrans)  
  sdHist=sd(gTrans)  
  runOtsu=TRUE  
    
  ##sd threshold
  
  ubSD = 5  
  tcSD = 3 
    
  ##unbalance check
  
  unbalanceUp=mHist+sdHist*ubSD<max(rHist$breaks)  
  unbalanceDown=mHist-sdHist*ubSD>min(rHist$breaks)  
  if(unbalanceUp+unbalanceDown>0)    
    runOtsu=FALSE
  
  ##too centralized check
  
   

  centralUp=mHist+sdHist*tcSD>max(rHist$breaks)
    
  centralDown=mHist-sdHist*tcSD<min(rHist$breaks)
  
  if(centralUp+centralDown >1)
    
    runOtsu=FALSE
  
  ##Otsu procedure  
  
  t=t+0.001  
  numPixel=length(allGreyValues)  
  ni=t  
  pi=ni/numPixel  
  uT=function(p,L){    
    res=0    
    for (i in 1:L){      
      res = res+i*p[i]      
    }    
    res    
  }
  
  wK = function(p,k){    
    res=0    
    for (i in 1:k){      
      res = res+p[i]      
    }    
    res    
  }
  
  uK = function(p,k){    
    res=0    
    for (i in 1:k){      
      res = res+i*p[i]      
    }    
    res    
  }
  
  maxT=function(p,L,k){    
    (uT(p,L)*wK(p,k)-uK(p,k))^2/(wK(p,k)*(1-wK(p,k)))    
  }
  
  
  if(length(t)>1){ 
    if(runOtsu){
      ##run Otsu  
      
      o=optimize(maxT,c(1,length(t)),maximum=TRUE,L=length(t),p=pi)      
      threshold=o$maximum/255      
    }    
    else{      
	  ##run sd
      
      threshold = mean(allGreyValues)+sd(allGreyValues)*sdbenchMark      
    }
    
    
  }else{    
     ##run sd
      
      threshold = mean(allGreyValues)+sd(allGreyValues)*sdbenchMark
  } 
}


##call Threshold function


threshold<-calculateOtsu(statistic$STATISTIC)
tmp <- as.data.frame(threshold)
colnames(tmp) <- c("THRESHOLD")
result <- tmp
END;


------Execution ATE Single Threshold Procedure based on Prediction Data

DROP PROCEDURE "PAPOC".RMODEL_ATE_PREDICT_THRESHOLD;
CREATE PROCEDURE "PAPOC".RMODEL_ATE_PREDICT_THRESHOLD()
LANGUAGE SQLSCRIPT AS

BEGIN


	 declare max_mat integer;
	 declare max_runid integer;
	 declare var_mat integer = 1;
	 declare var_runid integer = 1;
	 declare var_thd double;
	 declare var_max integer;
	 declare var_filter integer = 150;
	 --select top 1 RUNID into var_runid from "PAPOC"."RMODEL_R2_EXECUTION_TRACE" 
	 --where lastrun = 1;
	 
	 --restore table

	DROP TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD;
	CREATE COLUMN TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD(
	"MATERIAL_ID_INDEX" INTEGER, 
	--"ATE_INDEX" INTEGER,
	"THRESHOLD" DOUBLE,
	"RUNID" VARCHAR(50) 
	);
	
	--insert into  "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
	--select distinct MATERIAL_ID_INDEX, 0.14, 
	--(select top 1 RUNID from "PAPOC"."RMODEL_R2_EXECUTION_TRACE" where lastrun = 1) 
	--from "PAPOC"."RMODEL_MD_R2ITEM_ATE";
	

	 --threshold calculation
 	 
 	 result = 	select * from "PAPOC"."RMODEL_R2_RESULT_MEAN_ATE"; 	 
    
     
	 select max(distinct MATERIAL_ID_INDEX) into max_mat from :result;
	 
     while :var_mat <= :max_mat DO
     	select max(distinct TO_INTEGER(RUNID)) into max_runid from :result where MATERIAL_ID_INDEX = :var_mat;
     	var_runid = 1;
     	while :var_runid <= :max_runid DO
     		statistic = select MEAN as "STATISTIC" from :result where MATERIAL_ID_INDEX = :var_mat and RUNID = TO_VARCHAR(:var_runid);
     		CALL "PAPOC".RMODEL_USE_THRESHOLD_SINGLE_SRC(:statistic, threshold);
     		select THRESHOLD into var_thd from :threshold;
     		insert into  "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
     		values (:var_mat, :var_thd, TO_VARCHAR(:var_runid));
     		var_runid = :var_runid + 1;
     	END while;
     	var_mat = :var_mat + 1;     	
     END while;
     
	 --statistic = select FR as "STATISTIC" from :src where MATERIAL_ID_INDEX = 1;
	 --select * from :statistic;
	 --CALL "PAPOC".RMODEL_USE_THRESHOLD_SRC(:statistic, threshold);
	 --select * from :threshold;
END;

CALL "PAPOC".RMODEL_ATE_PREDICT_THRESHOLD()
select  * from "PAPOC"."RMODEL_PREDICTION_ATE_THRESHOLD"



------Execution Material Batch lot Single Threshold Procedure based on Prediction Data

DROP PROCEDURE "PAPOC".RMODEL_BL_PREDICT_THRESHOLD;
CREATE PROCEDURE "PAPOC".RMODEL_BL_PREDICT_THRESHOLD()
LANGUAGE SQLSCRIPT AS

BEGIN


	 declare max_mat integer;
	 declare max_runid integer;
	 declare var_mat integer = 1;
	 declare var_runid integer = 1;
	 declare var_thd double;
	 declare var_max integer;
	 declare var_filter integer = 150;
	
	 
	 --restore table

	DROP TABLE "PAPOC".RMODEL_PREDICTION_BL_THRESHOLD;
	CREATE COLUMN TABLE "PAPOC".RMODEL_PREDICTION_BL_THRESHOLD(
	"MATERIAL_ID_INDEX" INTEGER, 
	"THRESHOLD" DOUBLE,
	"RUNID" VARCHAR(50) 
	);
	
	--insert into  "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
	--select distinct MATERIAL_ID_INDEX, 0.14, 
	--(select top 1 RUNID from "PAPOC"."RMODEL_R2_EXECUTION_TRACE" where lastrun = 1) 
	--from "PAPOC"."RMODEL_MD_R2ITEM_ATE";
	

	 --threshold calculation

	 
 	 
 	 result = select * from "PAPOC"."RMODEL_R2_RESULT_MEAN_BL";
 	 	 	 	 
	 select max(distinct MATERIAL_ID_INDEX) into max_mat from :result;
	 
	 
     while :var_mat <= :max_mat DO
     	select max(distinct TO_INTEGER(RUNID)) into max_runid from :result where MATERIAL_ID_INDEX = :var_mat;
     	var_runid = 1;
     	while :var_runid <= :max_runid DO
     		statistic = select MEAN as "STATISTIC" from :result where MATERIAL_ID_INDEX = :var_mat and RUNID = TO_VARCHAR(:var_runid);
     		CALL "PAPOC".RMODEL_USE_THRESHOLD_SINGLE_SRC(:statistic, threshold);
     		select THRESHOLD into var_thd from :threshold;
     		insert into  "PAPOC".RMODEL_PREDICTION_BL_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
     		values (:var_mat, :var_thd, TO_VARCHAR(:var_runid));
     		var_runid = :var_runid + 1;
     	END while;
     	var_mat = :var_mat + 1;     	
     END while;
     
     
     
	 --statistic = select FR as "STATISTIC" from :src where MATERIAL_ID_INDEX = 1;
	 --select * from :statistic;
	 --CALL "PAPOC".RMODEL_USE_THRESHOLD_SRC(:statistic, threshold);
	 --select * from :threshold;
END;


CALL "PAPOC".RMODEL_BL_PREDICT_THRESHOLD()
select * from "PAPOC"."RMODEL_PREDICTION_BL_THRESHOLD"


------Execution ATE Threshold Procedure based on statistic data

DROP PROCEDURE "PAPOC".RMODEL_ATE_STATISTIC_THRESHOLD;
CREATE PROCEDURE "PAPOC".RMODEL_ATE_STATISTIC_THRESHOLD()
LANGUAGE SQLSCRIPT AS

BEGIN


	 declare max_mat integer;
	 declare max_runid integer;
	 declare var_mat integer = 1;
	 declare var_runid integer = 1;
	 declare var_thd double;
	 declare var_max integer;
	 declare var_filter integer = 150;
	 --select top 1 RUNID into var_runid from "PAPOC"."RMODEL_R2_EXECUTION_TRACE" 
	 --where lastrun = 1;
	 
	 --restore table

	DROP TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD;
	CREATE COLUMN TABLE "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD(
	"MATERIAL_ID_INDEX" INTEGER, 
	--"ATE_INDEX" INTEGER,
	"THRESHOLD" DOUBLE,
	"RUNID" VARCHAR(50) 
	);
	
	--insert into  "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
	--select distinct MATERIAL_ID_INDEX, 0.14, 
	--(select top 1 RUNID from "PAPOC"."RMODEL_R2_EXECUTION_TRACE" where lastrun = 1) 
	--from "PAPOC"."RMODEL_MD_R2ITEM_ATE";
	
	
	 --threshold calculation
 	 
 	 /*
 	 src = select total.MATERIAL_ID_INDEX, total.ATE_INDEX, IFNULL(RCNT,0) as RCNT, CNT, IFNULL(RCNT/CNT,0) as FR, total.RUNID  	 
 	 from ( select MATERIAL_ID_INDEX, ATE_INDEX, SUM(CNT) as CNT, RUNID 
 	 from (select MATERIAL_ID_INDEX, ATE_INDEX, T_CNT as CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_T" 
 	 union all select MATERIAL_ID_INDEX, TEST1_ATE_INDEX, FT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FT" 
 	 union all select MATERIAL_ID_INDEX, TEST2_ATE_INDEX, FT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FT" 
 	 union all select MATERIAL_ID_INDEX, TEST1_ATE_INDEX, FFT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFT" 
 	 union all select MATERIAL_ID_INDEX, TEST2_ATE_INDEX, FFT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFT" 
 	 union all select MATERIAL_ID_INDEX, TEST3_ATE_INDEX, FFT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFT" 
 	 union all select MATERIAL_ID_INDEX, TEST1_ATE_INDEX, FFF_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFF" 
 	 union all select MATERIAL_ID_INDEX, TEST2_ATE_INDEX, FFF_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFF" 
 	 union all select MATERIAL_ID_INDEX, TEST3_ATE_INDEX, FFF_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFF") 
 	 group by MATERIAL_ID_INDEX, ATE_INDEX,RUNID) total 
 	 left join ( select MATERIAL_ID_INDEX, ATE_INDEX, SUM(RCNT) as RCNT, RUNID 
 	 from (select MATERIAL_ID_INDEX, TEST1_ATE_INDEX as ATE_INDEX, FT_CNT as RCNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FT" 
 	 union all select MATERIAL_ID_INDEX, TEST1_ATE_INDEX, FFT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFT" 
 	 union all select MATERIAL_ID_INDEX, TEST2_ATE_INDEX, FFT_CNT, RUNID from "PAPOC"."RMODEL_INPUT_AGGR_FACT_FFT") 
 	 group by MATERIAL_ID_INDEX, ATE_INDEX, RUNID) fail 
 	 on total.MATERIAL_ID_INDEX = fail.MATERIAL_ID_INDEX and total.ATE_INDEX = fail.ATE_INDEX and total.RUNID = fail.RUNID;*/
 	 
 	 maxlist = select MATERIAL_ID_INDEX, RUNID, MAX(TOTAL_CNT) as MCNT from "PAPOC"."RMODEL_R2_RESULT_FACT_ATE" group by MATERIAL_ID_INDEX, RUNID;
 	 
 	 result = select  t1.MATERIAL_ID_INDEX, t1.ATE_INDEX,t1.RETEST_CNT, t1.TOTAL_CNT,t1.RETEST_RATE, t2.MCNT/t1.TOTAL_CNT as WT, t1.RUNID from "PAPOC"."RMODEL_R2_RESULT_FACT_ATE" as t1 
 	 inner join :maxlist as t2 on t1.MATERIAL_ID_INDEX = t2.MATERIAL_ID_INDEX and t1.RUNID = t2.RUNID;

 	 	 	 
	 
	 select max(distinct MATERIAL_ID_INDEX) into max_mat from :result;
	 
     while :var_mat <= :max_mat DO
     	select max(distinct TO_INTEGER(RUNID)) into max_runid from :result where MATERIAL_ID_INDEX = :var_mat;
     	var_runid = 1;
     	while :var_runid <= :max_runid DO
     		statistic = select RETEST_RATE as "STATISTIC" from :result where MATERIAL_ID_INDEX = :var_mat and RUNID = TO_VARCHAR(:var_runid) and WT < :var_filter;
     		CALL "PAPOC".RMODEL_USE_THRESHOLD_SRC(:statistic, threshold);
     		select THRESHOLD into var_thd from :threshold;
     		insert into  "PAPOC".RMODEL_PREDICTION_ATE_THRESHOLD (MATERIAL_ID_INDEX, THRESHOLD,RUNID) 
     		values (:var_mat, :var_thd, TO_VARCHAR(:var_runid));
     		var_runid = :var_runid + 1;
     	END while;
     	var_mat = :var_mat + 1;     	
     END while;
     
     
     
	 --statistic = select FR as "STATISTIC" from :src where MATERIAL_ID_INDEX = 1;
	 --select * from :statistic;
	 --CALL "PAPOC".RMODEL_USE_THRESHOLD_SRC(:statistic, threshold);
	 --select * from :threshold;
END;

CALL "PAPOC".RMODEL_ATE_STATISTIC_THRESHOLD()
select  * from "PAPOC"."RMODEL_PREDICTION_ATE_THRESHOLD"
select  * from "PAPOC"."RMODEL_PREDICTION_ATE_THRESHOLD" where RUNID = '10'
select  * from "PAPOC"."RMODEL_PREDICTION_ATE_THRESHOLD" where RUNID = 'test'

