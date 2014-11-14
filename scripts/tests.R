load_all()

banks <- loadManuallyLinkedBanks()

t1 <- collapseManuallyLinkedTable(dt = banks)

t2 <- t1[, lapply(.SD, collapseDuplicatesWithinString)]

t2[, bscode.unique := selectUniqueBSIndex(bscode)]

allbanks <- getAllBanks()
