pelanggan <- read.csv("https://storage.googleapis.com/dqlab-dataset/customer_segments.txt", sep="\t")

#Menampilkan data pada kolom :
pelanggan[c("Jenis.Kelamin", "Umur", "Profesi", "Tipe.Residen")]

#Buat variable field_yang_digunakan dengan isi berupa vector "Jenis.Kelamin", "Umur" dan "Profesi"
#field_yang_digunakan <- c("Jenis.Kelamin","Umur","Profesi")

#Tampilan data pelanggan dengan nama kolom sesuai isi vector field_yang_digunakan
#pelanggan[field_yang_digunakan]

#Konversi data menjadi numerik
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

#Penggabungan data
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#Mengisi data master
Profesi <- unique(pelanggan[c("Profesi", "Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin", "Jenis.Kelamin.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen", "Tipe.Residen.1")])

#Normalisasi Nilai
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000

field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1", "NilaiBelanjaSetahun")

#Bagian K-Means
#set.seed(100)
set.seed(1)

#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
#segmentasi <- kmeans(x = pelanggan[c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1", "NilaiBelanjaSetahun")], centers = 5, nstart = 25)
segmentasi <- kmeans(x = pelanggan[field_yang_digunakan], centers = 5, nstart = 25)

#Penggabungan hasil cluster
#Memebuat kolom baru bernama "cluster" di variabel pelanggan yg isinya dari segmentasi$cluster.
segmentasi$cluster
pelanggan$cluster = segmentasi$cluster
str(pelanggan)

#Analisa hasil
#Filter cluster ke-1
which(pelanggan$cluster == 1)

#Panjang hasil cluster 1-5
length(which(pelanggan$cluster == 1))
length(which(pelanggan$cluster == 2))
length(which(pelanggan$cluster == 3))
length(which(pelanggan$cluster == 4))
length(which(pelanggan$cluster == 5))

#Melihat data cluster ke 1-5
pelanggan[which(pelanggan$cluster == 1),]
pelanggan[which(pelanggan$cluster == 2),]
pelanggan[which(pelanggan$cluster == 3),]
pelanggan[which(pelanggan$cluster == 4),]
pelanggan[which(pelanggan$cluster == 5),]

#Melihat cluster means dari objek 
segmentasi$centers

#Membandingkan dengan 2 cluster kmeans, masing-masing 2 dan 5
set.seed(1)
kmeans(x=pelanggan[field_yang_digunakan], centers=2, nstart=25)
set.seed(1)
kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)

#Melihat nilai "Total Sum of Squares per cluster" :
segmentasi$withinss

#Melihat nilai "Vector dari cluster untuk tiap titik data" :
segmentasi$cluster

#Melihat nilai "Total penjumlahan dari tiap SS dari withinss" :
segmentasi$tot.withinss

#Bagian K-Means
set.seed(1)
sse <- sapply(1:10, function(param_k) {
  kmeans(pelanggan[field_yang_digunakan], param_k, nstart = 25)$tot.withinss
})
sse

library("ggplot2")

#Grafik elbow effect
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) + geom_line(color="red") + 
  geom_point() + ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") + 
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) + 
  scale_x_discrete(limits=c(1:jumlah_cluster_max))


#Lengkapi dengan dua vector bernama cluster dan Nama.Segmen

#Membuat satu variable data frame  bernama Segmen.Pelanggan yang terdiri dari dua kolom: 
#cluster: vector dengan isi 1,2,3,4 dan 5. Nama.Segmen: vector dengan isi "Silver Youth Gals", 
#"Diamond Senior Member", "Gold Young Professional", "Diamond Professional", dan "Silver Mid Professional".
Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), Nama.Segmen=c("Silver Youth Gals", 
                                                                   "Diamond Senior Member", 
                                                                   "Gold Young Professional", 
                                                                   "Diamond Professional", 
                                                                   "Silver Mid Professional"))

#Menggabungkan seluruh aset ke dalam variable Identitas.Cluster
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, Tipe.Residen=Tipe.Residen, 
                          Segmentasi=segmentasi, Segmen.Pelanggan=Segmen.Pelanggan, 
                          field_yang_digunakan=field_yang_digunakan)

#Menyimpan objek dalam bentuk file
saveRDS(Identitas.Cluster,"cluster.rds")

#Membuat data baru dengan memetakan data pelanggan baru
databaru <- data.frame(Customer_ID="CUST-100", Nama.Pelanggan="Rudi Wilamar",Umur=20,Jenis.Kelamin="Wanita",Profesi="Pelajar",Tipe.Residen="Cluster",NilaiBelanjaSetahun=3.5)
databaru

#Masukkan perintah untuk penggabungan data
databaru <- merge(databaru, Identitas.Cluster$Profesi)
databaru <- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas.Cluster$Tipe.Residen)

#menentukan data baru di cluster mana
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply(1:5, function(x)sum((databaru[Identitas.Cluster$field_yang_digunakan] - 
                                                                           Identitas.Cluster$Segmentasi$centers[x,])^2))),]

