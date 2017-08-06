<?php

date_default_timezone_set('Asia/Tokyo');

require_once(dirname(__FILE__) . '/database.php');

if (preg_match('%PHP 7.1.7 Development Server%', $_SERVER['SERVER_SOFTWARE'])) {
  Database::Initialize('proxy.sx9.jp', 10306, 'unagi', 'kExeDkJP');
} else {
  Database::Initialize('localhost', 3306, 'unagi', 'kExeDkJP');
}
ini_set('memory_limit', '1G');
