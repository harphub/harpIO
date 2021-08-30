import mysql.connector
import pandas as pd
import numpy as np
import datetime
import sqlite3
import os

try:
    mydb = mysql.connector.connect( host = 'srv-mondo.kol.shmu.sk',
                                    user = 'nwp',
                                    database = 'obs',
                                    charset='utf8')

except mysql.connector.Error as my_error:


    print(mydb_error.msg)  
    print(mydb_error)


############   set date range  ###############

start = os.getenv("start_date")
end = os.getenv("end_date")

#convert 'str' format in to datetime
start_date = datetime.datetime.strptime(start,"%Y%m%d%H")
end_date = datetime.datetime.strptime(end,"%Y%m%d%H")

start_date = start_date.strftime('%Y-%m-%d')
end_date = end_date.strftime('%Y-%m-%d')

###############################################
time_01 = ['00:00','01:00','02:00','03:00','04:00','05:00','06:00','07:00','08:00','09:00','10:00','11:00','12:00','13:00','14:00',
'15:00','16:00','17:00','18:00','19:00','20:00','21:00','22:00','23:00']
time_03 = ['00:00','03:00','06:00','09:00','12:00','15:00','18:00','21:00']
time_06 = ['00:00','06:00','12:00','18:00']
time_12 = ['00:00','12:00']


dates = [start_date]
print(dates)

# ##########   query database  ###################################################################

# ########    1h ######################

time1=dates[0]
#time2=dates[-1]

validdate01 = []
LON01 = []
LAT01 = []
RRR01 = []
ELEV01 = []
SID01 = []

for k in dates:
    for l in time_01:

        query01 = ("SELECT unix_timestamp(MAX(date_format(date,'%Y-%m-%d %H'))) AS validdate, si.ii AS SID,si.lon,si.lat,si.elev, ROUND(SUM(pr_sum),2) AS AccPcp1h,COUNT(pr_sum) AS n_pr,COUNT(*) AS n_obs "
        + " FROM obs.obs_sxsq39_1m AS o JOIN si.si ON o.si_id=si.id  WHERE si.si.ci=11 and date BETWEEN DATE_SUB('"+ k +" "+ l +"', INTERVAL 59 MINUTE) AND '"+ k +" "+ l +"' "
        + " GROUP BY si_id HAVING n_pr > 40 ORDER BY si.ii;")

        df_01 = pd.read_sql(query01,con = mydb)

        for RR01 in df_01.AccPcp1h:
            RRR01.append(RR01)
        for time01 in df_01.validdate:
            validdate01.append(time01)
        for Lon in df_01.lon:
            LON01.append(Lon)
        for Lat in df_01.lat:
            LAT01.append(Lat)
        for Elev in df_01.elev:
            ELEV01.append(Elev)
        for Sid in df_01.SID:
            SID01.append(Sid)     

r = {'validdate':validdate01,'SID':SID01,'lon':LON01,'lat':LAT01,'elev':ELEV01,'AccPcp1h':RRR01}
df_RRR01 = pd.DataFrame(r,columns=['validdate','SID','lon','lat','elev','AccPcp1h'])  
print(query01)
print("precipitation 1h:",df_RRR01)
##### 3h #######################################################################################

validdate03 = []
LON03 = []
LAT03 = []
RRR03 = []
ELEV03 = []
SID03 = []

for k in dates:
    for l in time_03:

        query03 = ("SELECT unix_timestamp(MAX(date_format(date,'%Y-%m-%d %H'))) AS validdate, si.ii AS SID,si.lon,si.lat,si.elev, ROUND(SUM(pr_sum),2) AS AccPcp3h,COUNT(pr_sum) AS n_pr,COUNT(*) AS n_obs "
        + " FROM obs.obs_sxsq39_1m AS o JOIN si.si ON o.si_id=si.id  WHERE si.si.ci=11 and date BETWEEN DATE_SUB('"+ k +" "+ l +"', INTERVAL 179 MINUTE) AND '"+ k +" "+ l +"' "
        + " GROUP BY si_id HAVING n_pr > 160 ORDER BY si.ii;")

        df_03 = pd.read_sql(query03,con = mydb)

        for RR03 in df_03.AccPcp3h:
            RRR03.append(RR03)
        for time03 in df_03.validdate:
            validdate03.append(time03)
        for Lon in df_03.lon:
            LON03.append(Lon)
        for Lat in df_03.lat:
            LAT03.append(Lat)
        for Elev in df_03.elev:
            ELEV03.append(Elev)
        for Sid in df_03.SID:
            SID03.append(Sid)     
        #print(query03)
        #print("validdate_3h:",df_03)
r = {'validdate':validdate03,'SID':SID03,'lon':LON03,'lat':LAT03,'elev':ELEV03,'AccPcp3h':RRR03}
df_RRR03 = pd.DataFrame(r,columns=['validdate','SID','lon','lat','elev','AccPcp3h'])  
#print("validdate_3h:",validdate03)
#print("precipitation 3h:",df_RRR03)
#### 6h ########################################################################################

validdate06 = []
LON06 = []
LAT06 = []
RRR06 = []
ELEV06 = []
SID06 = []

for k in dates:
    for l in time_06:

        query06 = ("SELECT unix_timestamp(MAX(date_format(date,'%Y-%m-%d %H'))) AS validdate, si.ii AS SID,si.lon,si.lat,si.elev, ROUND(SUM(pr_sum),2) AS AccPcp6h,COUNT(pr_sum) AS n_pr,COUNT(*) AS n_obs "
        + "FROM obs.obs_sxsq39_1m AS o JOIN si.si ON o.si_id=si.id  WHERE si.si.ci=11 and date BETWEEN DATE_SUB('"+ k +" "+ l +"', INTERVAL 359 MINUTE) AND '"+ k +" "+ l +"' "
        +  "GROUP BY si_id HAVING n_pr > 300 ORDER BY si.ii;")

        df_06 = pd.read_sql(query06,con = mydb)
    
        for RR06 in df_06.AccPcp6h:
            RRR06.append(RR06)
        for time06 in df_06.validdate:
            validdate06.append(time06)
        for Lon in df_06.lon:
            LON06.append(Lon)
        for Lat in df_06.lat:
            LAT06.append(Lat)
        for Elev in df_06.elev:
            ELEV06.append(Elev)
        for Sid in df_06.SID:
            SID06.append(Sid)
        print(query06)         
        print("data_frame --------------> ",df_06)
s = {'validdate':validdate06,'SID':SID06,'lon':LON06,'lat':LAT06,'elev':ELEV06,'AccPcp6h':RRR06}
df_RRR06 = pd.DataFrame(s,columns=['validdate','SID','lon','lat','elev','AccPcp6h'])       

print("precipitation 6h:",df_RRR06)
##### 12h #######################################################################################

validdate12 = []
LON12 = []
LAdates2 = []
RRR12 = []
ELEV12 = []
SID12 = []


for k in dates:
    for l in time_12:

        query12 = ("SELECT unix_timestamp(MAX(date_format(date,'%Y-%m-%d %H'))) AS validdate, si.ii AS SID,si.lon,si.lat,si.elev, ROUND(SUM(pr_sum),2) AS AccPcp12h,COUNT(pr_sum) AS n_pr,COUNT(*) AS n_obs  "
        + "FROM obs.obs_sxsq39_1m AS o JOIN si.si ON o.si_id=si.id  WHERE date BETWEEN DATE_SUB('"+ k +" "+ l +"', INTERVAL 719 MINUTE) AND '"+ k +" "+ l +"' "
        + "GROUP BY si_id HAVING n_pr > 700 ORDER BY si.ii;")

        df_12 = pd.read_sql(query12,con = mydb)
        
        for RR12 in df_12.AccPcp12h:
            RRR12.append(RR12)
        for time12 in df_12.validdate:
            validdate12.append(time12)
        for Lon in df_12.lon:
            LON12.append(Lon)
        for Lat in df_12.lat:
            LAdates2.append(Lat)
        for Elev in df_12.elev:
            ELEV12.append(Elev)
        for Sid in df_12.SID:
            SID12.append(Sid)     
        print(query12)
z = {'validdate':validdate12,'SID':SID12,'lon':LON12,'lat':LAdates2,'elev':ELEV12,'AccPcp12h':RRR12}
df_RRR12 = pd.DataFrame(z,columns=['validdate','SID','lon','lat','elev','AccPcp12h'])  

print("precipitation 12h:",df_RRR12)

data_0103 = df_RRR01.merge(df_RRR03,how='left')
data_010306 = data_0103.merge(df_RRR06,how='left')
data_01030612 = data_010306.merge(df_RRR12,how='left')


#query mondo database:
query=( "select unix_timestamp(date) AS validdate,ii as SID,lon,lat,elev,pa_avg AS Ps, ta_2m+273.15 AS T2m, rh_avg as RH2m, ws_avg as S10m, wd_avg as D10m from obs_sxsq39_1m join si.si on obs_sxsq39_1m.si_id=si.si.id "
     + "where date between '"+ time1 + " 00:00' and '"+ time1 + " 23:59' and  minute(date)='0'")

print(query)

df = pd.read_sql(query, con = mydb)

#calculate paraameters:
df['Pmsl']=((df['Ps'])*pow(1-(0.0065*df['elev']/(df['T2m']+0.0065*df['elev'])),-5.257))
df['Td2m']=243.04*(np.log(df['RH2m']/100)+((17.625*df['T2m'])/(243.04+df['T2m'])))/(17.625-np.log(df['RH2m']/100)-((17.625*df['T2m'])/(243.04+df['T2m'])))

print("Synop data:",df)

conn = sqlite3.connect('/data/users/ext005/products/harp/sql/observation/OBSTABLE_2021.sqlite')
#conn = sqlite3.connect('/work/users/ext005/sql/observations/OBSTABLE_2020.sqlite')
df.to_sql("SYNOP",conn,if_exists="append",index=False)
conn.commit()
conn.close()

print("SYNOP data saved in to SQLite")

conn1 = sqlite3.connect('/data/users/ext005/products/harp/sql/observation/zrazky/OBSTABLE_2021.sqlite')
#conn1 = sqlite3.connect('/work/users/ext005/sql/observations/zrazky/OBSTABLE_2020.sqlite')
data_01030612.to_sql("SYNOP",conn1,if_exists="append",index=False)
conn1.commit()
conn1.close()

#df_RRR06.to_csv("/work/users/ext005/sql/observations/zrazky/RRR06.csv",sep=",")
print("Precipitation data saved in to SQLite")
