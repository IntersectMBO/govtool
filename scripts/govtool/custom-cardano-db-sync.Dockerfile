FROM ghcr.io/intersectmbo/cardano-db-sync:13.5.0.2

COPY custom-cardano-db-sync.entrypoint.sh /usr/local/bin/custom-cardano-db-sync.entrypoint.sh
RUN chmod +x /usr/local/bin/custom-cardano-db-sync.entrypoint.sh

ENTRYPOINT [ "/usr/local/bin/custom-cardano-db-sync.entrypoint.sh" ]
