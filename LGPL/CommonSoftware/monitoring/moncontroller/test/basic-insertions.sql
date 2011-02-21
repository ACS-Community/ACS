INSERT INTO Configuration   VALUES(NULL, 'Test', 'Full name', TRUE, CURRENT_TIMESTAMP, 'Description');
INSERT INTO HwConfiguration VALUES(NULL, (SELECT ConfigurationID from Configuration), 0, 0, 0);
INSERT INTO ComponentType   VALUES(NULL, 'IDL:alma/Control/MountVertex.idl:1.0', NULL);
commit;
