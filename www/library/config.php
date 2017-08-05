<?php

date_default_timezone_set('Asia/Tokyo');

require_once(dirname(__FILE__) . '/database.php');

Database::Initialize('proxy.sx9.jp', 10306, 'unagi', 'kExeDkJP');
ini_set('memory_limit', '1G');
