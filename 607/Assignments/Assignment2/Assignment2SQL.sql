CREATE SCHEMA `database1` ;

CREATE TABLE `database1`.`movierankings` (
  `ID` INT NOT NULL,
  `Name` VARCHAR(45) NULL,
  `Movie` VARCHAR(45) NULL,
  `Rank` INT NULL,
  PRIMARY KEY (`ID`));


INSERT INTO `database1`.`movierankings` VALUES (1,"Joshua", "Lion King", 1);
INSERT INTO `database1`.`movierankings` VALUES (2,"Joshua", "Toy Story 4", 5);
INSERT INTO `database1`.`movierankings` VALUES (3,"Joshua", "Spider Man", 3);
INSERT INTO `database1`.`movierankings` VALUES (4,"Joshua", "John Wick 3", NULL);
INSERT INTO `database1`.`movierankings` VALUES (5,"Joshua", "Avengers End Game", 2);
INSERT INTO `database1`.`movierankings` VALUES (6,"Joshua", "Aladdin", 4);


INSERT INTO `database1`.`movierankings` VALUES (7,"Yara", "Lion King", 1);
INSERT INTO `database1`.`movierankings` VALUES (8,"Yara", "Toy Story 4", 3);
INSERT INTO `database1`.`movierankings` VALUES (9,"Yara", "Spider Man",NULL );
INSERT INTO `database1`.`movierankings` VALUES (10,"Yara", "John Wick 3", 2);
INSERT INTO `database1`.`movierankings` VALUES (11,"Yara", "Avengers End Game", 5);
INSERT INTO `database1`.`movierankings` VALUES (12,"Yara", "Aladdin", 4);


INSERT INTO `database1`.`movierankings` VALUES (13,"Mike", "Lion King", 3);
INSERT INTO `database1`.`movierankings` VALUES (14,"Mike", "Toy Story 4", NULL);
INSERT INTO `database1`.`movierankings` VALUES (15,"Mike", "Spider Man", 2);
INSERT INTO `database1`.`movierankings` VALUES (16,"Mike", "John Wick 3", NULL);
INSERT INTO `database1`.`movierankings` VALUES (17,"Mike", "Avengers End Game", 1);
INSERT INTO `database1`.`movierankings` VALUES (18,"Mike", "Aladdin", 4);


INSERT INTO `database1`.`movierankings` VALUES (19,"Xing", "Lion King", 2);
INSERT INTO `database1`.`movierankings` VALUES (20,"Xing", "Toy Story 4", 1);
INSERT INTO `database1`.`movierankings` VALUES (21,"Xing", "Spider Man", 3);
INSERT INTO `database1`.`movierankings` VALUES (22,"Xing", "John Wick 3", 4);
INSERT INTO `database1`.`movierankings` VALUES (23,"Xing", "Avengers End Game", 5);
INSERT INTO `database1`.`movierankings` VALUES (24,"Xing", "Aladdin", 6);


INSERT INTO `database1`.`movierankings` VALUES (25,"Dario", "Lion King", 3);
INSERT INTO `database1`.`movierankings` VALUES (26,"Dario", "Toy Story 4", 5);
INSERT INTO `database1`.`movierankings` VALUES (27,"Dario", "Spider Man", 2);
INSERT INTO `database1`.`movierankings` VALUES (28,"Dario", "John Wick 3", 4);
INSERT INTO `database1`.`movierankings` VALUES (29,"Dario", "Avengers End Game", 1);
INSERT INTO `database1`.`movierankings` VALUES (30,"Dario", "Aladdin", NULL);

