README

Cara deploy di RStudio:

1. Download semua dataset yang sudah disediakan.

2. Download file 'capstone_tetris_helmimrizal.Rmd'
di folder 'markdown'

3. Nyalakan RStudio, set working directory di folder
yang sama dengan dataset yang sudah di-download

4. Buka file 'capstone_tetris_helmimrizal.Rmd'

5. Coba Run setiap chunk dalam markdown
dengan meng-klik tombol segitiga hijau 
di kanan atas tiap chunk

Anda juga bisa Run seluruh markdown
dengan pilih 'Run for All'
atau pencet Ctrl + Shift + R

6. Jika sudah lancar, maka klik 'Knit'
di tengah atas markdown

Jika ada error, mungkin markdown bisa dicocokkan
dengan code yang ada di folder 'plot'

7. Jika sukses, akan muncul window baru,
Lalu klik 'Open in Browser' untuk membuka
di browser Internet Anda

### Tambahan ###

1. Harap bersabar saat melakukan deploy

2. Salah satu penyebab error adalah
working directory dan tempat dataset tidak sama.
Untuk mengatasinya, tulis direktori dataset
di bagian loading data pada markdown
(bagian read.csv(tulis direktori lengkap))
