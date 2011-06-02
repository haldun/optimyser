CREATE TABLE `log` (
  `experiment_id` char(36) collate utf8_bin NOT NULL,
  `combination_id` varchar(80) collate utf8_bin NOT NULL,
  `visitor_id` varchar(40) collate utf8_bin NOT NULL,
  `visitor_ip` varchar(15) collate utf8_bin NOT NULL,
  `created_on` timestamp NOT NULL,
  `page_type` varchar(10) collate utf8_bin NOT NULL,
  KEY `experiment_id` (`experiment_id`),
  KEY `visitor_id` (`visitor_id`),
  KEY `page_type` (`page_type`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_bin
