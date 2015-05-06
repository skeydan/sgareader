
set pages 0
set heading off
set trimspool on

select 'RN=' || rownum || ',ADDR=' || addr || ',SIZE=' || len from (select addr, to_number(lead(addr) over (order by addr),'XXXXXXXXXXXXXXXX') - to_number(addr,'XXXXXXXXXXXXXXXX') len from x$ksuse order by addr);
