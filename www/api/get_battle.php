<?php

require_once(dirname(__FILE__) . '/../library/api.php');

Database::Command('
    SELECT (@battle_id := battle_id) AS battle_id, battle_lock
    FROM battle NATURAL JOIN (
        SELECT DISTINCT battle_id
        FROM punter WHERE punter_score IS NULL) AS punter
    WHERE battle_lock <= NOW()
    ORDER BY battle_lock DESC LIMIT 1');
Database::Command('
    UPDATE battle
    SET battle_lock = NOW() + INTERVAL 5 SECOND
    WHERE battle_lock <= NOW() AND battle_id = @battle_id
    LIMIT 1');

if (Database::AffectedRows() != 1) {
  die('{}');
}

$battle = Database::SelectRow(
    'SELECT * FROM battle NATURAL JOIN map WHERE battle_id = @battle_id');
$battle['punter'] =
    Database::Select('SELECT * FROM punter NATURAL JOIN ai WHERE battle_id = @battle_id ORDER BY punter_id');
die(json_encode($battle));
