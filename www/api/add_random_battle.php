<?php

require_once(dirname(__FILE__) . '/../library/api.php');

$map = Database::SelectRow('
    SELECT map_id, map_capacity, RAND() / map_weight AS map_weight
    FROM map ORDER BY map_weight LIMIT 1');

$ais = Database::Select('
    SELECT ai_id, ai_key, RAND() / ai_weight AS ai_weight
    FROM (SELECT * FROM ai UNION ALL SELECT * FROM ai) AS ai
    ORDER BY ai_weight LIMIT {limit}',
    ['limit' => intval($map['map_capacity'])]);

Database::Command('INSERT INTO battle SET map_id = {map_id}', $map);
$battle_id = Database::InsertId();
if ($battle_id === FALSE) {
  die(json_encode(['error' => 'Failed to insert.']));
}

$punters = [];
foreach ($ais as $ai) {
  $punters[] = ['battle_id' => $battle_id, 'ai_id' => $ai['ai_id']];
}
Database::Command('INSERT INTO punter {punters}', ['punters' => $punters]);

die(json_encode(['map' => $map, 'punters' => $punters]));
