#hamps in order by category vids 
batch_files_dir <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/batch_files/"
cat_hamps_dir <- '/Volumes/startech3TB_bkup/research_data/Pic_Vid/hamp_files/videos/category'

pleasant_hamps <-   list.files(cat_hamps_dir, 
                                            pattern = "at1.hamp8$",
                                            full.names = T)

neutral_hamps <-    list.files(cat_hamps_dir, 
                                            pattern = "at2.hamp8$",
                                            full.names = T)

unpleasant_hamps <-                 list.files(cat_hamps_dir, 
                                            pattern = "at3.hamp8$",
                                            full.names = T)



hamps <- c(pleasant_hamps,
           neutral_hamps,
           unpleasant_hamps)

write.table(x = hamps,
            file = paste0(batch_files_dir, "vid_hamps_by_category_by_participant.rep"),
            quote = F,
            row.names = F,
            col.names = F)


#hamps in order by category pics
batch_files_dir <- "/Volumes/startech3TB_bkup/research_data/Pic_Vid/batch_files/"
cat_ar_dir <- '/Volumes/startech3TB_bkup/research_data/Pic_Vid/averaged_files/pics/by_category'

pleasant_ar <-   list.files(cat_ar_dir, 
                               pattern = "at1.ar$",
                               full.names = T)

neutral_ar <-    list.files(cat_ar_dir, 
                               pattern = "at2.ar$",
                               full.names = T)

unpleasant_ar <-                 list.files(cat_ar_dir, 
                                               pattern = "at3.ar$",
                                               full.names = T)

ars <- c(pleasant_ar,
           neutral_ar,
           unpleasant_ar)

write.table(x = ars,
            file = paste0(batch_files_dir, "pics_ar_by_category_by_par.rep"),
            quote = F,
            row.names = F,
            col.names = F)


