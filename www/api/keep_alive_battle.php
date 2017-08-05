<?php

require_once(dirname(__FILE__) . '/../library/api.php');

Database::Command('
    UPDATE battle
    SET
      battle_lock = NOW() + INTERVAL {interval} SECOND
    WHERE
      battle_id = {battle_id} AND
      battle_lock < NOW() + INTERVAL {interval} SECOND
    LIMIT 1',
    ['interval' => intval($_GET['interval']),
     'battle_id' => intval($_GET['battle_id'])]);

die(json_encode(['affected_rows' => Database::AffectedRows()]));
