# Skrip by: supmawatimeysi@gmail.com
# Memuat library yang diperlukan
library(tidyverse)   # Untuk manipulasi data dan visualisasi
library(lubridate)   # Untuk menangani tanggal
library(treemapify)  # Untuk membuat visualisasi treemap
library(waffle)      # Untuk membuat visualisasi waffle chart

# Mengatur locale untuk menghindari error terkait tanggal
Sys.setlocale(locale="C")

# Membaca dataset dari file CSV
df <- read_csv('C:/Users/lenio/Downloads/Sample - Superstore.csv/Sample - Superstore.csv')

# Melihat 6 data teratas dari dataset
head(df)

# Menampilkan jumlah baris dan kolom dari dataset
cat('Total of rows:', nrow(df))
cat('\nTotal of columns:', ncol(df))

# Visualisasi data 1: Rata-rata kecepatan pengiriman berdasarkan mode pengiriman
df <- df %>%
  mutate(`Order Date` = mdy(`Order Date`),  # Mengubah format tanggal Order Date
         `Ship Date` = mdy(`Ship Date`),    # Mengubah format tanggal Ship Date
         'Shipping Speed' = `Ship Date` - `Order Date`)  # Menghitung kecepatan pengiriman

df %>%
  group_by(`Ship Mode`) %>%  # Mengelompokkan berdasarkan Ship Mode
  summarize(mean=mean(`Shipping Speed`)) %>%  # Menghitung rata-rata kecepatan pengiriman
  ggplot(aes(x=reorder(`Ship Mode`, -`mean`), y=`mean`, fill=reorder(`Ship Mode`, `mean`)))+
  geom_bar(stat='identity')+
  coord_flip()+  # Membalik sumbu untuk mempermudah pembacaan
  geom_text(aes(label = paste0(round(mean, 1), ' day')), hjust = -0.5, size=5, fontface='bold')+
  scale_y_continuous(limits = c(0,6))+  # Membatasi skala sumbu y
  theme_classic()+
  labs(title='Comparison of Shipping Speed\nfor Each Ship Mode',
       x='Ship Mode',
       y='Mean (day)',
       fill='Ship Mode')+
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))

# Visualisasi data 2: Frekuensi Ship Mode per Segment
df %>% 
  ggplot(aes(x=`Ship Mode`, y=`Segment`, shape=`Segment`, color=`Segment`))+
  geom_count()+  # Menampilkan jumlah dengan ukuran lingkaran
  scale_size(range = c(3,18))+
  theme_light()+
  labs(title='Ship Mode per Segment')+
  theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5))

# Visualisasi data 3: Top 5 pelanggan berdasarkan total barang yang dibeli
customer <- df %>%
  group_by(`Customer Name`) %>%
  summarize('Total Item'=sum(Quantity)) %>%
  arrange(-`Total Item`) %>%  # Mengurutkan dari yang terbanyak
  slice(1:5)  # Memilih 5 pelanggan teratas

customer %>%
  ggplot(aes(x=reorder(`Customer Name`, `Total Item`), y=`Total Item`, color=`Customer Name`))+
  geom_point(stat='identity', size=12)+
  geom_segment(aes(y=0, xend=`Customer Name`, yend=`Total Item`))+
  geom_text(aes(label=`Total Item`), color='white', size=5, vjust=0.5, fontface='bold')+
  theme_classic()+
  labs(title='Top 5 Customers\nby Total Purchased Items')+
  theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))+
  scale_y_continuous(limits=c(0, 155))+
  coord_flip()

# Menyiapkan data untuk visualisasi tambahan terkait segmen pelanggan
customerName <- customer$'Customer Name'
segment <- df[df$'Customer Name' %in% customerName,]

segment %<>%
  group_by(`Customer Name`, `Segment`) %>%
  summarize()

left_join(x=customer, y=segment, by='Customer Name')

# Visualisasi data 4: Top 5 pelanggan berdasarkan total pembayaran
customerSales <- df %>%
  group_by(`Customer Name`) %>%
  summarize('Total Payment'=sum(Sales), 'Total Item'=sum(Quantity)) %>%
  arrange(-`Total Payment`) %>%
  slice(1:5)

customerSales %>%
  ggplot(aes(x=reorder(`Customer Name`, `Total Payment`), y=`Total Payment`, fill=`Customer Name`))+
  geom_bar(stat='identity')+
  geom_text(aes(label=paste0(round(`Total Payment`), '\nUSD')), color='white', size=5, vjust=2, fontface='bold')+
  theme_classic()+
  labs(title='Top 5 Customers\nby Total Payment')+
  theme(plot.title=element_text(size=20, hjust=0.5, face='bold'))+
  scale_y_continuous(limits=c(0, 26000))

# Visualisasi data 5
# Visualisasi total penjualan berdasarkan kategori menggunakan diagram lingkaran
df %>%
  group_by(Category) %>%  # Mengelompokkan data berdasarkan Category
  summarize('Total Sales' = sum(Sales)) %>%  # Menghitung total penjualan per kategori
  ggplot(aes(x="", y=`Total Sales`, fill=Category))+
  geom_bar(stat="identity", width=1)+  # Membuat bar chart
  coord_polar("y", start=0)+  # Mengubah bar chart menjadi diagram lingkaran
  labs(title='Total Sales by Category')+  # Menambahkan judul
  theme_void()+  # Menghilangkan sumbu dan elemen lain untuk tampilan bersih
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 6
# Visualisasi profit berdasarkan sub-kategori dalam setiap kategori menggunakan stacked bar chart
df %>%
  group_by(Category, `Sub-Category`) %>%  # Mengelompokkan data berdasarkan Category dan Sub-Category
  summarize('Total Profit' = sum(Profit)) %>%  # Menghitung total profit
  ggplot(aes(x=Category, y=`Total Profit`, fill=`Sub-Category`))+
  geom_bar(stat="identity", position='stack')+  # Membuat stacked bar chart
  labs(title='Total Profit by Sub-Categories within Each Category')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 7
# Scatter plot untuk melihat hubungan antara Penjualan dan Profit, diwarnai berdasarkan Segment
df %>%
  ggplot(aes(x=Sales, y=Profit, color=Segment))+
  geom_point(size=3, alpha=0.6)+  # Scatter plot dengan transparansi
  labs(title='Sales vs Profit by Segment')+  # Menambahkan judul
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 8
# Boxplot untuk menganalisis sebaran nilai Quantity berdasarkan Segment
df %>%
  ggplot(aes(x=Segment, y=Quantity, fill=Segment))+
  geom_boxplot()+  # Membuat boxplot
  labs(title='Quantity Distribution by Segment')+  # Menambahkan judul
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 9
# Heatmap untuk melihat korelasi antara Sales, Profit, dan Quantity dalam setiap sub-kategori
df %>%
  group_by(`Sub-Category`) %>%
  summarize(Sales=sum(Sales), Profit=sum(Profit), Quantity=sum(Quantity)) %>%
  ggplot(aes(x=Sales, y=Profit, fill=Quantity))+
  geom_tile(color="white")+  # Membuat heatmap
  scale_fill_gradient(low="lightblue", high="blue")+  # Skema warna untuk jumlah Quantity
  labs(title='Heatmap of Sales, Profit, and Quantity\nby Sub-Categories')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 10
# Visualisasi line plot untuk melihat tren total penjualan bulanan
df %>%
  group_by(`Order Date` = floor_date(`Order Date`, "month")) %>%  # Mengelompokkan data per bulan
  summarize(`Total Sales` = sum(Sales)) %>%  # Menghitung total penjualan bulanan
  ggplot(aes(x=`Order Date`, y=`Total Sales`))+
  geom_line(color="blue", size=1.5)+  # Membuat line plot
  labs(title='Monthly Sales Trend')+  # Menambahkan judul
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 11
# Memvisualisasikan total profit bulanan berdasarkan kategori menggunakan area plot
df %>%
  group_by(Category, `Order Date` = floor_date(`Order Date`, "month")) %>%  # Mengelompokkan per kategori dan bulan
  summarize(`Total Profit` = sum(Profit)) %>%  # Menghitung total profit bulanan
  ggplot(aes(x=`Order Date`, y=`Total Profit`, fill=Category))+
  geom_area(alpha=0.6, position='stack')+  # Membuat area plot
  labs(title='Monthly Profit Trend\nby Category')+  # Menambahkan judul
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 12
# Plot scatter antara Sales dan Profit, menambahkan garis halus untuk setiap Category
df %>%
  ggplot(aes(x=Sales, y=Profit))+
  geom_point(size=3, alpha=0.4)+  # Titik scatter dengan transparansi
  geom_smooth(aes(color=Category), method='gam', fullrange = TRUE)+  # Garis halus untuk masing-masing kategori menggunakan metode GAM
  facet_wrap(~Category)+  # Membuat panel terpisah berdasarkan Category
  labs(title='Sales vs Profit\nfor Each Category')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 13
# Visualisasi maksimum diskon yang diberikan pada masing-masing kategori menggunakan waffle chart
df %>%
  group_by(Category) %>%
  summarize('Max Discount'=max(Discount)*10) %>%  # Menghitung diskon maksimum per kategori
  ggplot(aes(fill=Category, values=`Max Discount`))+
  geom_waffle(n_rows = 2, size = 4, color = "white", flip=TRUE)+  # Waffle chart dengan 2 baris per kategori
  geom_text(aes(x=1.5, y=2.5, label=paste0(`Max Discount`*10, '%')), size=15, alpha=0.5, fontface='bold')+
  facet_wrap(~Category)+  # Membuat panel terpisah berdasarkan kategori
  labs(title='Max Discount\nfor Each Category')+  # Judul grafik
  theme_void()+  # Menghapus sumbu dan elemen lain
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

df %>%
  group_by(Category, `Sub-Category`) %>%
  summarize('Max Discount'=max(Discount)*100) %>%
  arrange(-`Max Discount`) %>%  # Mengurutkan data berdasarkan diskon maksimum
  slice(1)  # Mengambil satu baris pertama dengan diskon tertinggi

# Visualisasi data 14
# Membandingkan hubungan antara diskon dan profit menggunakan bin2d plot
df %>%
  ggplot(aes(x=Discount, y=Profit))+
  geom_bin2d(size=3)+  # Menggunakan bin2d plot untuk melihat distribusi data
  labs(title='Discount vs Profit')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 15
# Histogram untuk melihat hubungan antara diskon dan profit, dipisahkan berdasarkan sub-kategori
df %>%
  ggplot(aes(x=Discount, y=Profit, color=Category, fill=Category))+
  geom_histogram(stat='identity', bins=10)+  # Histogram berdasarkan jumlah diskon dan profit
  facet_wrap(~`Sub-Category`)+  # Membuat panel terpisah berdasarkan sub-kategori
  labs(title='Discount vs Profit\nby Sub-Categories')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 16
# Menggunakan plot hex untuk melihat hubungan antara diskon dan sales
df %>%
  ggplot(aes(x=Discount, y=Sales))+
  geom_hex(size=3)+  # Plot hex untuk melihat densitas data pada grafik
  labs(title='Discount vs Sales')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 17
# Visualisasi total item yang diberikan diskon di atas 25% berdasarkan kategori
df_disc <- df[df$Discount >= 0.25, ]  # Filter data untuk diskon lebih dari atau sama dengan 25%

df_disc %>%
  ggplot(aes(x=Discount, fill=`Sub-Category`))+
  geom_bar()+  # Menghitung jumlah item dengan diskon lebih dari 25%
  facet_wrap(~Category)+  # Panel terpisah berdasarkan kategori
  labs(title='Total discounted item above 25%\nby Categories')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

# Visualisasi data 18
# Plot densitas dua dimensi antara Quantity dan Profit untuk melihat pola distribusi
df %>%
  ggplot(aes(x=Quantity, y=Profit))+
  geom_density_2d()+  # Menambahkan garis kontur densitas
  stat_density2d(aes(fill = ..level..), geom = "polygon")+  # Plot polygon untuk densitas data
  labs(title='Quantity vs Profit')+  # Judul grafik
  theme_light()+  # Tema ringan
  theme(plot.title=element_text(size=20, face='bold', hjust=0.5))  # Penyesuaian tampilan judul

