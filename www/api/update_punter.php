<?php

require_once(dirname(__FILE__) . '/../library/api.php');

Database::Command('
    UPDATE punter
    SET punter_score = {punter_score}, punter_pass = {punter_pass}
    WHERE punter_id = {punter_id}
    LIMIT 1',
    ['punter_score' => intval($_GET['punter_score']),
     'punter_id' => intval($_GET['punter_id']),
     'punter_pass' => @intval($_GET['punter_pass'])]);

die(json_encode(['affected_rows' => Database::AffectedRows()]));
