set pages 0
set heading off echo off trimspool on feedback off
spool ksuse.txt
select 'SGA_BASE=' || addr from x$ksmmem where indx=0;
select chr(10) from dual;
select 'RN=' || rownum || ',ADDR=' || addr || ',SIZ=' || len from (select addr, to_number(lead(addr) over (order by addr),'XXXXXXXXXXXXXXXX') - to_number(addr,'XXXXXXXXXXXXXXXX') len from x$ksuse order by addr);
select chr(10) from dual;
select b.kqfconam || '=' || b.kqfcooff || ',' ||  b.kqfcosiz from x$kqfta a, x$kqfco b where a.indx = b.kqfcotab and a.kqftanam= 'X$KSUSE' and b.kqfcooff > 0 order by b.kqfcooff;
spool off
