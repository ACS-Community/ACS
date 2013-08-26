1,$ g/MText `BEGIN/ /<String `/,/^  >$/-1 m -3\
+1 s/^.*<Marker/  >\
 > # end of Para\
 <Para\
  <PgfTag `Body'>\
  <ParaLine\
   <TextInset/\
+1 s/MType 25/Unique 402/\
+1 s/MText `BEGIN /TiSrcFile `<c\\>/\
+1 s/MCurrPage .*>/TiLastUpdate  869579668 763612>/

1,$ g/<TiLastUpdate/ +1 s/<Unique.*>/<TiAutoUpdate Yes>/\
+1 s/> # end of Marker/ <TiFlow\
     <TiMainFlow Yes>\
     <TiFormatting TiEnclosing>\
     <TiFormatRemovePageBreaks No>\
     <TiFormatRemoveOverrides No>\
    > # end of TiFlow\
   > # end of TextInset/

1,$ g/<MText `[0-9]*: ManHeading:/ -2,+3 m ?<String `?-1

1,$ g/MText `END/ -2,+2 d\
. s/> # end of Marker/ <TextInsetEnd >/

1,$ g/<String `.*BEGIN IMPORTED FILE:/ d
1,$ g/<String `.*END IMPORTED FILE:/ d
wq
