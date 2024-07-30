FROM ghcr.io/intersectmbo/cardano-db-sync:sancho-5.1.0

COPY custom-cardano-db-sync.entrypoint.sh /usr/local/bin/custom-cardano-db-sync.entrypoint.sh
RUN chmod +x /usr/local/bin/custom-cardano-db-sync.entrypoint.sh

ENTRYPOINT [ "/usr/local/bin/custom-cardano-db-sync.entrypoint.sh" ]
