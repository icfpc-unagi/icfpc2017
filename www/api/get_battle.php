<?php

require_once(dirname(__FILE__) . '/../library/api.php');

Database::Command('
    UPDATE battle
    SET
      battle_id = (@battle_id := battle_id),
      battle_lock = NOW() + INTERVAL 5 SECOND
    WHERE
      battle_lock <= NOW() AND
      battle_id IN (
        SELECT DISTINCT battle_id
        FROM punter WHERE punter_score IS NULL)
    ORDER BY battle_lock
    LIMIT 1');

if (Database::AffectedRows() != 1) {
  die('{}');
}

$battle = Database::SelectRow(
    'SELECT * FROM battle NATURAL JOIN map WHERE battle_id = @battle_id');
$battle['punter'] =
    Database::Select('SELECT * FROM punter NATURAL JOIN ai WHERE battle_id = @battle_id');
die(json_encode($battle));
