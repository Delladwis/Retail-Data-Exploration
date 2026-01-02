#Load data dan simpan ke dalam variable bernama ‘data’
data <- read.csv("https://storage.googleapis.com/dqlab-dataset/transaksi_stok_dan_penjualan.tsv", 
                 header = TRUE, sep = "\t")

#Tampilkan 5 data teratas 
head(data,5)

#Tampilkan 5 data terbawah 
tail(data,5)

#Tampilkan informasi mengenai struktur dari data
str(data)

#Ubah tipe data variabel Tanggal menjadi date
data$Tanggal <- as.Date(data$Tanggal, "%d-%m-%Y")
#Cek apakah tipe data dari variabel Tanggal sudah menjadi date
str(data$Tanggal)
#Tambahkan kolom baru untuk menyimpan data bulan dan tahun
data$Bulan_Tahun <- format(data$Tanggal, "%m-%Y")

#Tampilkan 5 data teratas
head(data, 5)

#Ubah tipe data variabel Harga menjadi numerik
data$Harga <- as.numeric(data$Harga)
#Ubah data NA menjadi 0
data$Harga[is.na(data$Harga)] <- 0
#Cek apakah tipe data dari variabel Harga sudah menjadi tipe numerik
str(data$Harga)
#Tampilkan 5 data teratas
head(data, 5)
#Lalu ambillah data dengan jenis transaksi adalah Penjualan
data_penjualan = data[data$Jenis.Transaksi=="Penjualan",]

#Lakukan fungsi aggregasi data untuk mendapatkan penjualan perbulan
penjualan_perbulan = aggregate(x=data_penjualan$Jumlah, 
                               by = list(Bulan_Tahun=data_penjualan$Bulan_Tahun),
                               FUN = sum)

#Keluarkan bar plot dari penjualan perbulan
barplot(penjualan_perbulan$x,
        names.arg =penjualan_perbulan$Bulan_Tahun,
        xlab="Month",
        ylab="Penjualan",
        col="yellow",
        main="Penjualan perbulan",
        border="orange")
penjualan_perbulan

#Penjualan Mitra Toserba Xera pada nulan April-Juni 2020 semakin menurun. 
#Penjualan Tertinggi yang dimiliki oleh Mitra Toserba Xera berada pada bulan April 2020

#Keluarkan data dengan jenis transaksi adalah Penjualan
data_penjualan = data[data$Jenis.Transaksi == "Penjualan",]

#Lakukan fungsi aggregasi data untuk mendapatkan pembelian per customer
pembelian_pelanggan=aggregate(
  x=data_penjualan$Jumlah,
  by =list(Pelanggan=data_penjualan$Nama.Pelanggan),
  FUN = sum)
pembelian_pelanggan

#Urutkan data pelanggan berdasarkan jumlah pembelian dari yang terbesar ke yang terkecil
pembelian_pelanggan = pembelian_pelanggan[order(-pembelian_pelanggan$x), ]

#Ambil 10 nilai tertinggi dari data diatas
head(pembelian_pelanggan, 10)

#Perbandingan barang masuk dan keluar perbulan
aggregate(
  x=data$Jumlah, 
  by = list(Bulan = data$Bulan_Tahun, Jenis_Transaksi = data$Jenis.Transaksi), 
  FUN = sum)

#Visualisasikan data dengan chart yang sesuai
#Buat tabel transaksi menggunakan fungsi aggregate
data_transaksi = aggregate(
  x=data$Jumlah, 
  by = list(Bulan = data$Bulan_Tahun, Jenis_Transaksi = data$Jenis.Transaksi), 
  FUN = sum)
data_transaksi

#Keluarkan data transaksi penjualan dan stok masuk
data_penjualan <- data_transaksi[(data_transaksi$Jenis_Transaksi) == "Penjualan",]
data_stok_masuk <- data_transaksi[(data_transaksi$Jenis_Transaksi) == "Stok Masuk",]

#Gabungkan kedua data diatas menggunakan fungsi merge dengan left join
data_gabungan = merge(data_stok_masuk,data_penjualan,by='Bulan', all.x=TRUE)
data_gabungan = data.frame(Bulan = data_gabungan$Bulan,
                           Stok_Masuk = data_gabungan$x.x,
                           Penjualan = data_gabungan$x.y)

#Periksa apakah terdapat NA data. Jika terdapat NA data, kamu dapat menggantinya dengan 0
data_gabungan$Penjualan[is.na(data_gabungan$Penjualan)] <- 0

#Ubah format data gabungan dengan melakukan perintah transpose. Lalu ubah nama kolom menggunakan bulan
data_gabung = t(as.matrix(data_gabungan[-1]))
colnames(data_gabung) = data_gabungan$Bulan

#Keluarkan bar plot dengan multiple kategori untuk membandingkan stok masuk dengan penjualan. Lalu keluarkan legend dari barplot tersebut.
barplot(data_gabung,
        main='Perbandingan Penjualan dengan Stok Masuk',
        ylab='Jumlah Barang', 
        xlab='Bulan',
        beside = TRUE, 
        col=c("blue","cyan"))
legend('topright',fill=c("blue","cyan"),legend=c('Stok Masuk','Penjualan'))




#Analisis hubungan antara Harga Barang dengan Jumlah Transaksi
#Memilih data dengan jenis transaksi Penjualan
data <- data[(data$Jenis.Transaksi) == "Penjualan",]

#Mengubah data harga menjadi Integer
data$Harga <- as.integer(data$Harga)

#Mengubah nilai NA menjadi 0
data$Harga[is.na(data$Harga)] <- 0

#Menghitung jumlah transaksi berdasarkan rentang harga
data_transaksi <- aggregate(
  x=data$No.Transaksi, 
  by = list(Harga = data$Harga), 
  FUN = length)

#Mengurutkan data dari harga termahal
data_transaksi = data_transaksi[order(-data_transaksi$Harga), ]
data_transaksi

#Visualisasi data hubungan harga barang dengan jumlah transaksi
#Sebelum menggunakan perintah hist() kamu perlu memecah data transaksi diatas menjadi bentuk data vektor sebagai berikut
data_transaksi_freq = as.vector(rep(data_transaksi$Harga, data_transaksi$x))

#Setelah mendapatkan data diatas, kita dapat mengeluarkan histogram dari tabel diatas dengan menggunakan perintah hist()
hist(data_transaksi_freq,
     main="Hubungan antara harga barang dengan transaksi",
     xlab="Rentang harga barang",
     col="blue"
)

# Dari hasil yang kudapatkan, aku dapat mengetahui bahwa barang yang berada pada rentang harga Rp 10.000-Rp15.000 
# lebih banyak terjual dibanding rentang harga lainnya. Setelah itu, barang dengan harga kurang dari Rp 5000 menempati posisi kedua. 
# Aku dapat mengambil kesimpulan bahwa barang yang berada di rentang harga ini merupakan barang yang paling laku.
