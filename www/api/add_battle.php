<?php

require_once(dirname(__FILE__) . '/../library/api.php');

$battle_map = $_GET['battle_map'] ?: 'sample';

if (!in_array($battle_map, ['boston-sparse', 'circle', 'edinburgh-sparse', 'gothenburg-sparse', 'lambda', 'nara-sparse', 'oxford-center-sparse', 'oxford2-sparse-2', 'randomMedium', 'randomSparse', 'sample', 'Sierpinski-triangle', 'tube', 'van-city-sparse'])) {
  die(json_encode(['error' => 'Unknown map: ' . $battle_map]));
}

if (!isset($_GET['ai_key']) || !is_array($_GET['ai_key']) ||
    count($_GET['ai_key']) == 0) {
  die(json_encode(['error' => 'AI key must be a non-empty array.']));
}
foreach ($_GET['ai_key'] as $ai_key) {
  $ai_keys[] = '"' . $ai_key . '"';
}

$ais = [];
foreach (Database::Select('SELECT ai_key, ai_id FROM ai WHERE ai_key IN (' . implode(', ', $ai_keys) . ')') as $ai) {
  $ais[$ai['ai_key']] = $ai['ai_id'];
}

$ai_ids = [];
foreach ($_GET['ai_key'] as $ai_key) {
  if (!isset($ais[$ai_key])) {
    die(json_encode(['error' => 'No such AI: ' . $ai_key]));
  }
  $ai_ids[] = $ais[$ai_key];
}

Database::Command(
    'INSERT INTO battle {battle}',
    ['battle' => ['battle_map' => $battle_map]]);
$battle_id = Database::InsertId();
if ($battle_id === FALSE) {
  die(json_encode(['error' => 'Failed to insert.']));
}

$punters = [];
foreach ($ai_ids as $ai_id) {
  $punters[] = ['battle_id' => $battle_id, 'ai_id' => $ai_id];
}
Database::Command('INSERT INTO punter {punters}', ['punters' => $punters]);

die(json_encode($punters));
