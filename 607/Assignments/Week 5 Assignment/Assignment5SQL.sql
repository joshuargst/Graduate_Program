CREATE SCHEMA `database1` ;

CREATE TABLE `database1`.`assignment5table` (
  `ID` INT NOT NULL,
  `Airline` VARCHAR(45) NULL,
  `Arrival_Status` VARCHAR(45) NULL,
  `Los Angeles` INT NULL,
  `Phoenix` INT NULL,
  `San Diego` INT NULL,
  `San Francisco` INT NULL,
  `Seattle` INT NULL, 
  PRIMARY KEY (`ID`));

INSERT INTO `database1`.`assignment5table` VALUES (1,"ALASKA","on time", 497, 221,212,503,1841);
INSERT INTO `database1`.`assignment5table` VALUES (2,"ALASKA","delayed", 62,12,20,102,305);
INSERT INTO `database1`.`assignment5table` VALUES (3,"AM West","on time",694,4840,383,320,201);
INSERT INTO `database1`.`assignment5table` VALUES (4,"AM West","delayed",117,415,65,129,61);
