
library(testthat)
library(httr)

source("Vize_Q1_210401059_Mertcan_Caliskan.R")  # script.R'nin gerçek yoluyla değiştirin

test_that("Spotify Token Tests", {
  # Test 1.1) Global Workspace’de spotify_token adlı bir değişken olmalı.
  expect_true(exists("spotify_token"))
  
  # Test 1.2) spotify_token adlı değişkenin tipi “function” olmalı.
  expect_equal(typeof(spotify_token), "closure")
  
  # Test 1.3) spotify_token() çağrıldığında döndürdüğü çıktı bir liste olmalı
  result <- spotify_token()
  expect_true(is.list(result))
  
  # Test 1.4) spotify_token() çağrıldığında döndürdüğü listenin iki elementi olmalı
  expect_equal(length(result), 2)
  
  # Test 1.5) spotify_token() çağrıldığında döndürdüğü listenin ilk elementinin ismi status_code olmalı
  expect_equal(names(result)[1], "status_code")
  
  # Test 1.6) spotify_token() çağrıldığında döndürdüğü listenin ilk elementinin class’ı numeric olmalı
  expect_equal(as.numeric(result$status_code), 200)
  
  # Test 1.7) spotify_token() çağrıldığında döndürdüğü listenin status_code adlı elementinin değeri 200’e eşit olmalı
  expect_equal(result$status_code, 200)
  
  # Test 1.8) spotify_token() çağrıldığında döndürdüğü listenin ikinci elementinin ismi token olmalı
  expect_equal(names(result)[2], "token")
  
  # Test 1.9) spotify_token() çağrıldığında döndürdüğü listenin ikinci elementinin class’ı character olmalı
  expect_equal(class(result$token), "character")
  
  # Test 1.10) spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi ’Bearer ’ ile başlamalı
  expect_true(startsWith(result$token, "Bearer "))
  
  # Test 1.11) spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi character değişkeninin içinde 122 adet harf bulunmalı
  expect_equal(nchar(result$token), 122)
})
