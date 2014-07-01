#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <pthread.h>
#include <strings.h>

void *run(void *ptr) {
    struct sockaddr_in serv_addr;
    int retval = 0;
    int sockfd = *(int *)ptr;
    
    bzero((char *) &serv_addr, sizeof(serv_addr));
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = inet_addr("9.9.9.9");
    serv_addr.sin_port = htons(22);

    printf("Connecting\n");
    retval = connect(sockfd, (const struct sockaddr *)&serv_addr, sizeof(serv_addr));

    if (retval < 0) 
        perror("Connect failed:\n");
    else 
        printf("Connect succeeded\n");
}

int main() {
    pthread_t t;
    int retval;
    int sockfd = socket(AF_INET, SOCK_STREAM, 0);


    retval = pthread_create( &t, NULL, run, (void *)&sockfd);

    sleep(5);
    printf("Closing sockfd\n");
    close(sockfd);
    printf("Closed sockfd\n");
    pthread_join(t, NULL);
    exit(0);
}


