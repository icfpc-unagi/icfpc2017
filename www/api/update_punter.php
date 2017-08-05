<?php

require_once(dirname(__FILE__) . '/../library/api.php');

Database::Command('
    UPDATE punter
    SET punter_score = {punter_score}
    WHERE punter_id = {punter_id}
    LIMIT 1',
    ['punter_score' => intval($_GET['punter_score']),
     'punter_id' => intval($_GET['punter_id'])]);

die(json_encode(['affected_rows' => Database::AffectedRows()]));
