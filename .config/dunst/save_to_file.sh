#!/bin/bash
#DUNST_APP_NAME, DUNST_SUMMARY, DUNST_BODY

echo -e $(date '+%Y-%m-%d %H:%M:%S \n') "$DUNST_APP_NAME\n $DUNST_SUMMARY\n $DUNST_BODY \n-------------------------------------------\n" >> /var/log/dunst/notifications.log